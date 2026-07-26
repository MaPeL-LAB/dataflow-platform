#Requires -Version 5.1

[CmdletBinding()]
param(
    [string]$Project = "",
    [Alias("Input")]
    [string[]]$InputPaths = @(),
    [string]$OutputRoot = "",
    [ValidateSet("version", "replace")]
    [string]$Mode = "version",
    [string]$Comment = "",
    [ValidateSet("true", "false")]
    [string]$OpenReport = "false",
    [switch]$Interactive,
    [switch]$DryRun,
    [Parameter(ValueFromRemainingArguments = $true)]
    [string[]]$PipelineArgs = @()
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$RepoRoot = $PSScriptRoot
$CallerDirectory = (Get-Location).ProviderPath
$InteractiveMode = $Interactive.IsPresent -or $PSBoundParameters.Count -eq 0
$MetadataProfileChanged = $false
$ProjectDescription = ""
$AuthorCitation = ""
$LicenseValue = ""
$AccessClassification = ""
$AccessConditions = ""
$TagsValue = ""

function Stop-DataFlow {
    param([Parameter(Mandatory = $true)][string]$Message)
    throw $Message
}

function Get-ProjectSlug {
    param([Parameter(Mandatory = $true)][string]$Value)
    $slug = $Value.ToLowerInvariant() -replace "[^a-z0-9]+", "-"
    $slug = $slug.Trim("-")
    if ([string]::IsNullOrWhiteSpace($slug)) { return "project" }
    return $slug
}

function Read-WithDefault {
    param(
        [Parameter(Mandatory = $true)][string]$Label,
        [AllowEmptyString()][string]$CurrentValue = ""
    )
    $prompt = if ([string]::IsNullOrWhiteSpace($CurrentValue)) {
        "${Label}"
    } else {
        "${Label} [${CurrentValue}]"
    }
    $entered = Read-Host $prompt
    if ($entered -eq "-") { return "" }
    if ([string]::IsNullOrWhiteSpace($entered)) { return $CurrentValue }
    return $entered.Trim()
}

function Invoke-CheckedR {
    param(
        [Parameter(Mandatory = $true)][string]$RscriptPath,
        [Parameter(Mandatory = $true)][string[]]$Arguments,
        [switch]$CaptureOutput
    )
    if ($CaptureOutput) {
        $output = & $RscriptPath @Arguments
        if ($LASTEXITCODE -ne 0) {
            Stop-DataFlow "R command failed with exit code ${LASTEXITCODE}."
        }
        return $output
    }
    & $RscriptPath @Arguments
    if ($LASTEXITCODE -ne 0) {
        Stop-DataFlow "R command failed with exit code ${LASTEXITCODE}."
    }
}

function Load-ProjectMetadata {
    param(
        [Parameter(Mandatory = $true)][string]$RscriptPath,
        [Parameter(Mandatory = $true)][string]$HelperPath,
        [Parameter(Mandatory = $true)][string]$ProfilePath
    )
    $jsonLines = Invoke-CheckedR -RscriptPath $RscriptPath -Arguments @(
        "--vanilla", $HelperPath, "read-json", $ProfilePath
    ) -CaptureOutput
    $metadata = (($jsonLines | Out-String).Trim() | ConvertFrom-Json)
    $script:ProjectDescription = [string]$metadata.project_description
    $script:AuthorCitation = [string]$metadata.author
    $script:LicenseValue = [string]$metadata.license
    $script:AccessClassification = [string]$metadata.access_classification
    $script:AccessConditions = [string]$metadata.access_permissions
    $script:TagsValue = [string]$metadata.tags
}

function Edit-ProjectMetadata {
    param(
        [Parameter(Mandatory = $true)][string]$RscriptPath,
        [Parameter(Mandatory = $true)][string]$HelperPath,
        [Parameter(Mandatory = $true)][string]$ProfilePath,
        [Parameter(Mandatory = $true)][string]$ProjectName,
        [Parameter(Mandatory = $true)][string]$ProjectDirectory,
        [switch]$IsDryRun
    )
    Write-Host "Complete the public metadata profile. Press Return to keep a displayed value."
    Write-Host "Enter a single hyphen (-) to clear a saved value."
    $script:ProjectDescription = Read-WithDefault "Project description" $script:ProjectDescription
    $script:AuthorCitation = Read-WithDefault "Author / preferred citation" $script:AuthorCitation
    $script:LicenseValue = Read-WithDefault "License" $script:LicenseValue
    $script:AccessClassification = Read-WithDefault "Access classification" $script:AccessClassification
    $script:AccessConditions = Read-WithDefault "Access conditions" $script:AccessConditions
    $script:TagsValue = Read-WithDefault "Tags (comma-separated)" $script:TagsValue

    if ($IsDryRun) {
        Write-Host "[DRY RUN] Would save project metadata: ${ProfilePath}"
        return
    }

    New-Item -ItemType Directory -Path $ProjectDirectory -Force | Out-Null
    Invoke-CheckedR -RscriptPath $RscriptPath -Arguments @(
        "--vanilla",
        $HelperPath,
        "write",
        $ProfilePath,
        $ProjectName,
        $script:ProjectDescription,
        $script:AuthorCitation,
        $script:LicenseValue,
        $script:AccessClassification,
        $script:AccessConditions,
        $script:TagsValue
    ) | Out-Null
    $script:MetadataProfileChanged = $true
    Write-Host "[INFO] Saved reusable project metadata: ${ProfilePath}"
}

function Configure-ProjectMetadata {
    param(
        [Parameter(Mandatory = $true)][string]$RscriptPath,
        [Parameter(Mandatory = $true)][string]$HelperPath,
        [Parameter(Mandatory = $true)][string]$ProfilePath,
        [Parameter(Mandatory = $true)][string]$ProjectName,
        [Parameter(Mandatory = $true)][string]$ProjectDirectory,
        [switch]$IsDryRun
    )
    if (Test-Path -LiteralPath $ProfilePath -PathType Leaf) {
        Load-ProjectMetadata $RscriptPath $HelperPath $ProfilePath
        Write-Host "[INFO] Found saved project metadata: ${ProfilePath}"
        $reuse = Read-Host "Reuse it without changes? [Y/n]"
        if ($reuse -match "^(n|no)$") {
            Edit-ProjectMetadata $RscriptPath $HelperPath $ProfilePath $ProjectName $ProjectDirectory -IsDryRun:$IsDryRun
        } else {
            Write-Host "[INFO] Reusing the saved public metadata profile."
        }
        return
    }

    $create = Read-Host "Create a reusable public metadata profile for this project? [Y/n]"
    if ($create -match "^(n|no)$") {
        Write-Host "[INFO] Continuing without a saved public metadata profile."
    } else {
        Edit-ProjectMetadata $RscriptPath $HelperPath $ProfilePath $ProjectName $ProjectDirectory -IsDryRun:$IsDryRun
    }
}

function Format-Command {
    param([Parameter(Mandatory = $true)][string[]]$Parts)
    return ($Parts | ForEach-Object {
        if ($_ -match '\s' -or $_.Contains("'")) {
            "'" + $_.Replace("'", "''") + "'"
        } else {
            $_
        }
    }) -join " "
}

function Test-ReparsePoint {
    param([Parameter(Mandatory = $true)][string]$Path)
    $item = Get-Item -LiteralPath $Path -Force -ErrorAction SilentlyContinue
    if ($null -eq $item) { return $false }
    return (($item.Attributes -band [System.IO.FileAttributes]::ReparsePoint) -ne 0)
}

function Set-CurrentVersion {
    param(
        [Parameter(Mandatory = $true)][string]$CurrentPath,
        [Parameter(Mandatory = $true)][string]$PointerPath,
        [Parameter(Mandatory = $true)][string]$VersionPath,
        [Parameter(Mandatory = $true)][string]$RunId
    )
    $currentItem = Get-Item -LiteralPath $CurrentPath -Force -ErrorAction SilentlyContinue
    if ($null -ne $currentItem) {
        if (-not (Test-ReparsePoint $CurrentPath)) {
            Stop-DataFlow "Refusing to replace a non-link current path: ${CurrentPath}"
        }
        Remove-Item -LiteralPath $CurrentPath -Force
    }

    $isWindows = $env:OS -eq "Windows_NT"
    $linkType = if ($isWindows) { "Junction" } else { "SymbolicLink" }
    try {
        New-Item -ItemType $linkType -Path $CurrentPath -Target $VersionPath -Force | Out-Null
    } catch {
        Write-Warning "Could not create the optional '${CurrentPath}' link. The portable current.txt pointer will still be updated. $($_.Exception.Message)"
    }

    Set-Content -LiteralPath $PointerPath -Value "versions/${RunId}" -Encoding UTF8
}

function Open-LocalReport {
    param([Parameter(Mandatory = $true)][string]$Path)
    if ($env:OS -eq "Windows_NT") {
        Start-Process -FilePath $Path
        return
    }
    if (Test-Path -LiteralPath "/System/Library/CoreServices") {
        & open -a Safari $Path
        return
    }
    $xdgOpen = Get-Command "xdg-open" -ErrorAction SilentlyContinue
    if ($null -ne $xdgOpen) {
        & $xdgOpen.Source $Path
        return
    }
    $gio = Get-Command "gio" -ErrorAction SilentlyContinue
    if ($null -ne $gio) {
        & $gio.Source open $Path
        return
    }
    Write-Warning "Could not locate a default-browser opener. Report: ${Path}"
}

try {
    Write-Host "[INFO] Preflight: validating local runtime and inputs."
    $rscriptCommand = Get-Command "Rscript" -ErrorAction SilentlyContinue
    if ($null -eq $rscriptCommand) {
        Stop-DataFlow "Required command not found: Rscript. Install R 4.1 or newer and add Rscript to PATH."
    }
    $RscriptPath = $rscriptCommand.Source

    $PipelineScript = Join-Path $RepoRoot "scripts/run_pipeline.R"
    $RegistryFile = Join-Path $RepoRoot "config/pipelines.yml"
    $MetadataHelper = Join-Path $RepoRoot "scripts/manage_project_metadata.R"
    $RuntimeCheck = Join-Path $RepoRoot "scripts/check_runtime.R"
    foreach ($requiredFile in @($PipelineScript, $RegistryFile, $MetadataHelper, $RuntimeCheck)) {
        if (-not (Test-Path -LiteralPath $requiredFile -PathType Leaf)) {
            Stop-DataFlow "Required repository file not found: ${requiredFile}"
        }
    }

    if ([string]::IsNullOrWhiteSpace($OutputRoot)) {
        $OutputRoot = Join-Path $RepoRoot "outputs"
    }

    if ($InteractiveMode) {
        while ([string]::IsNullOrWhiteSpace($Project)) {
            $Project = Read-Host "Project/study name"
        }
        if ($InputPaths.Count -eq 0) {
            Write-Host "Enter input files or directories. Press Return on an empty line when finished."
            $collectedInputs = @()
            while ($true) {
                $inputValue = Read-Host "Input path"
                if ([string]::IsNullOrWhiteSpace($inputValue)) { break }
                $collectedInputs += $inputValue
            }
            $InputPaths = $collectedInputs
        }

        $outputValue = Read-Host "Output root [${OutputRoot}]"
        if (-not [string]::IsNullOrWhiteSpace($outputValue)) { $OutputRoot = $outputValue }

        Write-Host "Run mode:"
        Write-Host "  1) New dated version (leave an existing current version unchanged)"
        Write-Host "  2) Replace current safely (retain the prior dated version)"
        $modeValue = Read-Host "Choose 1 or 2 [1]"
        switch ($modeValue) {
            "2" { $Mode = "replace" }
            "" { $Mode = "version" }
            "1" { $Mode = "version" }
            default { Stop-DataFlow "Run mode must be 1 or 2." }
        }

        $Comment = Read-Host "Optional run comment"
        $openValue = Read-Host "Open the completed open-science metadata report? [Y/n]"
        $OpenReport = if ($openValue -match "^(n|no)$") { "false" } else { "true" }
    }

    if ([string]::IsNullOrWhiteSpace($Project)) {
        Stop-DataFlow "-Project is required in non-interactive mode."
    }
    if ($InputPaths.Count -eq 0) {
        Stop-DataFlow "At least one -InputPaths value is required."
    }

    $ResolvedInputs = @()
    foreach ($inputPath in $InputPaths) {
        if (-not (Test-Path -LiteralPath $inputPath)) {
            Stop-DataFlow "Input path does not exist: ${inputPath}"
        }
        $ResolvedInputs += (Resolve-Path -LiteralPath $inputPath).ProviderPath
    }

    if (-not [System.IO.Path]::IsPathRooted($OutputRoot)) {
        $OutputRoot = Join-Path $CallerDirectory $OutputRoot
    }
    $OutputRoot = [System.IO.Path]::GetFullPath($OutputRoot)

    Invoke-CheckedR -RscriptPath $RscriptPath -Arguments @("--vanilla", $RuntimeCheck) | Out-Null

    $ProjectSlug = Get-ProjectSlug $Project
    $ProjectDirectory = Join-Path $OutputRoot $ProjectSlug
    $VersionsDirectory = Join-Path $ProjectDirectory "versions"
    $CurrentPath = Join-Path $ProjectDirectory "current"
    $CurrentPointer = Join-Path $ProjectDirectory "current.txt"
    $HistoryFile = Join-Path $ProjectDirectory "run_history.csv"
    $ProjectMetadataFile = Join-Path $ProjectDirectory "project_metadata.yml"

    if ($InteractiveMode) {
        Configure-ProjectMetadata $RscriptPath $MetadataHelper $ProjectMetadataFile $Project $ProjectDirectory -IsDryRun:$DryRun
    }

    $RunId = [DateTime]::UtcNow.ToString("yyyy-MM-ddTHHmmssZ")
    $VersionDirectory = Join-Path $VersionsDirectory $RunId
    $counter = 1
    while (Test-Path -LiteralPath $VersionDirectory) {
        $RunId = "{0}-{1:D2}" -f [DateTime]::UtcNow.ToString("yyyy-MM-ddTHHmmssZ"), $counter
        $VersionDirectory = Join-Path $VersionsDirectory $RunId
        $counter += 1
    }

    $RArguments = @("--vanilla", "scripts/run_pipeline.R", "--pipeline", "data_dictionary")
    foreach ($inputPath in $ResolvedInputs) {
        $RArguments += @("--input", $inputPath)
    }

    $customConfigProvided = $false
    foreach ($pipelineArgument in $PipelineArgs) {
        if ($pipelineArgument -eq "--config" -or $pipelineArgument -match "^--config=") {
            $customConfigProvided = $true
        }
    }
    if ((Test-Path -LiteralPath $ProjectMetadataFile -PathType Leaf) -and -not $customConfigProvided) {
        $RArguments += @("--config", $ProjectMetadataFile)
    } elseif ((Test-Path -LiteralPath $ProjectMetadataFile -PathType Leaf) -and $customConfigProvided) {
        Write-Warning "A custom --config was supplied; it takes precedence over the saved project metadata profile."
    }

    $RArguments += @(
        "--output", $VersionDirectory,
        "--project-name", $Project,
        "--run-comment", $Comment,
        "--overwrite", "false"
    )
    if ($PipelineArgs.Count -gt 0) { $RArguments += $PipelineArgs }

    Write-Host "[INFO] Project: ${Project}"
    Write-Host "[INFO] Run ID: ${RunId}"
    Write-Host "[INFO] Mode: ${Mode}"
    Write-Host "[INFO] Planned output: ${VersionDirectory}"

    if ($DryRun) {
        Write-Host "[DRY RUN] No run directories or artifacts will be created."
        Write-Host "[DRY RUN] Pipeline command:"
        Write-Host ("  " + (Format-Command (@($RscriptPath) + $RArguments)))
        Write-Host "Changed: nothing (dry run)."
        Write-Host "Next command: rerun without -DryRun when the paths and mode are correct."
        exit 0
    }

    Write-Host "[INFO] Creating project output directories."
    New-Item -ItemType Directory -Path $VersionsDirectory -Force | Out-Null

    Write-Host "[INFO] Running the local R pipeline."
    Push-Location $RepoRoot
    try {
        & $RscriptPath @RArguments
        $pipelineExitCode = $LASTEXITCODE
    } finally {
        Pop-Location
    }
    if ($pipelineExitCode -ne 0) {
        Stop-DataFlow "Pipeline failed. Any partial output remains at: ${VersionDirectory}"
    }

    $requiredArtifacts = @(
        "data_dictionary.xlsx",
        "data_dictionary.json",
        "metadata.xlsx",
        "metadata.json",
        "metadata_report.html",
        "open_science_metadata_report.html",
        "artifact_manifest.csv"
    )
    foreach ($artifact in $requiredArtifacts) {
        $artifactPath = Join-Path $VersionDirectory $artifact
        if (-not (Test-Path -LiteralPath $artifactPath -PathType Leaf) -or (Get-Item -LiteralPath $artifactPath).Length -eq 0) {
            Stop-DataFlow "Verification failed; missing or empty artifact: ${artifactPath}"
        }
    }

    $hasCurrent = (Test-Path -LiteralPath $CurrentPointer -PathType Leaf) -or
        ($null -ne (Get-Item -LiteralPath $CurrentPath -Force -ErrorAction SilentlyContinue))
    $BecameCurrent = $false
    if (-not $hasCurrent) {
        Write-Host "[INFO] No current version exists; promoting this first run."
        Set-CurrentVersion $CurrentPath $CurrentPointer $VersionDirectory $RunId
        $BecameCurrent = $true
    } elseif ($Mode -eq "replace") {
        Write-Host "[INFO] Promoting the new run; the prior dated version is retained."
        Set-CurrentVersion $CurrentPath $CurrentPointer $VersionDirectory $RunId
        $BecameCurrent = $true
    } else {
        Write-Host "[INFO] Created a dated version; the existing current version is unchanged."
    }

    $historyRow = [pscustomobject][ordered]@{
        run_id = $RunId
        completed_utc = [DateTime]::UtcNow.ToString("yyyy-MM-ddTHH:mm:ssZ")
        project_name = $Project
        project_slug = $ProjectSlug
        mode = $Mode
        comment = $Comment
        version_directory = $VersionDirectory
        became_current = $BecameCurrent.ToString().ToLowerInvariant()
    }
    if (Test-Path -LiteralPath $HistoryFile -PathType Leaf) {
        $historyRow | Export-Csv -LiteralPath $HistoryFile -NoTypeInformation -Append -Encoding UTF8
    } else {
        $historyRow | Export-Csv -LiteralPath $HistoryFile -NoTypeInformation -Encoding UTF8
    }

    $PublicReport = Join-Path $VersionDirectory "open_science_metadata_report.html"
    if ($OpenReport -eq "true") {
        Write-Host "[INFO] Opening the open-science metadata report."
        Open-LocalReport $PublicReport
    }

    Write-Host "Changed:"
    Write-Host "  - Created verified run: ${VersionDirectory}"
    Write-Host "  - Updated history: ${HistoryFile}"
    if ($MetadataProfileChanged) {
        Write-Host "  - Saved project metadata: ${ProjectMetadataFile}"
    } elseif (Test-Path -LiteralPath $ProjectMetadataFile -PathType Leaf) {
        Write-Host "  - Reused project metadata: ${ProjectMetadataFile}"
    }
    if ($BecameCurrent) {
        Write-Host "  - Current version pointer: ${CurrentPointer}"
    } else {
        Write-Host "  - Current version left unchanged: ${CurrentPointer}"
    }
    Write-Host "Open-science report: ${PublicReport}"
    Write-Host "Technical report: $(Join-Path $VersionDirectory 'metadata_report.html')"
    Write-Host ('Next command: Start-Process "{0}"' -f $PublicReport)
} catch {
    Write-Error "[ERROR] $($_.Exception.Message)"
    exit 1
}
