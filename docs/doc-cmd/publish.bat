rem fsformatting version 3.1.0
rem This is a fork of FSharp.Formatting which parses .fs (not .fsx) files. 
rem Compiled files are targetted to net461 and simply copied over to the applicable project.

@echo off

call rm -fr docs/doc-output && mkdir docs/doc-output
call docs/doc-tools/fsformatting.exe literate --processDirectory --inputDirectory "src" --outputDirectory "docs/doc-output" --templateFile "docs/doc-assets/templates/template.html"  --replacements "page-author" "Your name" "project-name" "Learning F#"
