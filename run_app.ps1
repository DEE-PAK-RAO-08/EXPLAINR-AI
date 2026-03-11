$Rscript = "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
$LogFile = "app.log"

if (Test-Path $LogFile) { Remove-Item $LogFile }

Write-Output "Starting EXPLAINR-AI..."
Start-Process -FilePath $Rscript -ArgumentList "app.R" -WorkingDirectory "c:\Users\LENOVO\Downloads\EXPLAINR-AI" -RedirectStandardOutput $LogFile -RedirectStandardError $LogFile -NoNewWindow

Write-Output "Waiting for app to start..."
$started = $false
for ($i = 0; $i -lt 120; $i++) {
    if (Test-Path $LogFile) {
        $content = Get-Content $LogFile
        if ($content -match "Listening on http") {
            Write-Output "App started successfully!"
            $content | Select-String "Listening on http"
            $started = $true
            break
        }
        if ($content -match "Error" -or $content -match "Execution halted") {
            Write-Output "App failed to start. Check app.log for details."
            $content | Select-String "Error" | Select-Object -Last 10
            break
        }
    }
    Start-Sleep -Seconds 5
}

if (-not $started) {
    Write-Output "Timed out waiting for app to start or it failed without explicit error in log."
}
