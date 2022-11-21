# Ataxx-Game

## So what is Ataxx?
Wikipedia Article - [Ataxx Wiki](https://en.wikipedia.org/wiki/Ataxx "Ataxx Wikipedia Site")

Please also read rules about how game works in this article.

In current version of game, 
* The board is 7X7.
* White will always Player-1 and Black is player-2
* White will have first move
* Clicking on your piece will show you your possible moves in red circle

## What is Haskell? 

Haskell is a computer programming language. In particular, it is a polymorphically statically typed, lazy, purely functional language, quite different from most other programming languages. 
<br>
<br>
More information is available at - [HaskellWiki](https://wiki.haskell.org/Introduction "Haskell Wiki")

## What is CodeWorld API?

The GUI of this game is built with CodeWorld API.

<br> [CodeWorld Module](https://hackage.haskell.org/package/codeworld-api-0.6.0) provides the drawing code for CodeWorld. It is heavily inspired by Gloss, but modified for consistency and pedagogical reasons.

## How to setup Haskell environment for this game

To run this project in **Windows Environment**,
Enter following command in *Windows Powershell*,

`Start-Transcript -Append ([Environment]::GetFolderPath("Desktop") + "\HaskellInstallLog.txt");$ExistingHaskellErrMsg = "Existing Haskell installation detected... aborting installation";try {ghc; gcm ghc; Write-Host $ExistingHaskellErrMsg -ForegroundColor Red; Stop-Transcript; Return} catch {};try {cabal; gcm cabal; Write-Host $ExistingHaskellErrMsg -ForegroundColor Red; Stop-Transcript; Return} catch {};if ((Get-CimInstance win32_logicaldisk)[0]."FreeSpace" -lt 5368709120) {Write-Host "Not enough space on C: drive (need at least 5GB)... aborting installation" -ForegroundColor Red; Stop-Transcript; Return};$WorkingDir = (Get-Location).Path;cd C:\;Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072;Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $false,$true,$true,$false,$false,$false,$true;$env:Path += ";C:\ghcup\bin";ghcup install ghc 8.6.5;ghcup set ghc 8.6.5;ghcup install cabal;$PathToAdd = ";C:\ghcup\msys64\usr\bin;C:\cabal\bin";$UserPath = [Environment]::GetEnvironmentVariables("User")::Path;[Environment]::SetEnvironmentVariable("Path", $UserPath + $PathToAdd, "User");$env:Path += $PathToAdd;$env:CABAL_DIR = "C:\cabal";cabal update;cabal install --lib codeworld-api-0.6.0 --constraint 'reflex <0.6.4';cabal install doctest;Write-Host "Haskell installation has finished!" -ForegroundColor Green;Stop-Transcript;cd $WorkingDir`

## Ok so you finished setting up environment, now how to run Game?

Well, game is setup with Haskell build tool named *cabal*.
All the requirements of project are written in [attax-haskell.cabal](./attax-haskell.cabal). You can read it if you want, but you do not need to understand it to run this game. 

Open the program directory in *Windows Powershell*.

To run the game with **Two Human Players**, write the following argument in powershell: <br>
`cabal v2-run game -- --p1 human --p2 human`

<br>

Single player can also play against AI. There are three AI designed in this game:<br>
* Easy (stuartBloom)
* Medium (greedy)
* Hard (default)

*More information on how to set up difficulty level of CPU*:
1. Easy - Easy difficulty CPU or as I like to call it [Stuart Bloom AI](https://bigbangtheory.fandom.com/wiki/Stuart_Bloom), is recommended CPU difficulty for **beginner players**. To play with this difficulty, replace *human* with *ai:stuartBloom* in in two human players setup argument. <br> For example, if you want to be player-1 and AI to be player-2 then, <br>
`cabal v2-run game -- --p1 human --p2 ai:stuartBloom`

2. Medium - Easy difficulty CPU or as I like to call it [howard Wolowitz AI](https://bigbangtheory.fandom.com/wiki/Howard_Wolowitz?page=1), is harder to play against compared to Stuart Bloom. This AI is based on **Greedy Algorithm**. To play with this difficulty, write following command line argument in *powershell* <br>
`cabal v2-run game -- --p1 human --p2 ai:greedy`

3. Hard - This is hardest CPU difficulty in this game. I like to call it [Raj Koothrappali AI](https://bigbangtheory.fandom.com/wiki/Rajesh_Koothrappali). This is also **default** AI in this game. So, if player does not specify AI name, difficulty of game will be set as hard. For example, <br>
`cabal v2-run game -- --p1 human --p2 ai` <br>
will run the game on Hard difficulty. This AI is based on **Alpha Beta Pruning** technique to play best CPU move.


#### I hope you have fun playing this game. I might add better AI in future


