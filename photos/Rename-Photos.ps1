# Rename-Photos.ps1
# Run this in the folder where your original photos are

$renames = @{
    "welding-fence (1).webp" = "welding-railing-01.webp"
    "welding-fence (5).webp" = "welding-railing-02.webp"
    "IMG_0769.webp"          = "welding-railing-03.webp"
    "IMG_0770.webp"          = "welding-railing-03.webp"   # Comment this line out if you prefer 0769
    "IMG_1588.JPEG"          = "security-door-sunburst-01.jpg"
    "IMG_1618.JPEG"          = "security-gate-01.jpg"
    "plantHanger.webp"       = "plant-hanger-01.webp"
    "tv2.webp"               = "tv-mounting-02.webp"
    "counterAfter.webp"      = "kitchen-counter-01.webp"
    "me.webp"                = "me.webp"
    "me.png"                 = "me.webp"                  # Only if you want to convert the png version
}

foreach ($old in $renames.Keys) {
    if (Test-Path $old) {
        Rename-Item -Path $old -NewName $renames[$old] -Force
        Write-Host "Renamed: $old → $($renames[$old])" -ForegroundColor Green
    } else {
        Write-Host "Skipped (not found): $old" -ForegroundColor Yellow
    }
}

Write-Host "`nDone! Now create a folder called 'photos' and move all the new renamed files into it." -ForegroundColor Cyan