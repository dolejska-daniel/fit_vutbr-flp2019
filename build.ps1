Write-Output "Creating build directory..."
New-Item -ItemType Directory -Path build -Force > $null

Write-Output "Building binary..."
Set-Location src
ghc --make -Wall -j -O2 -o ../rka-2-dka.exe -odir ../build -hidir ../build Main.hs

Set-Location ..
Write-Output "Ready!"
