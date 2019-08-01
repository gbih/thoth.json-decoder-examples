# fsformatting version 3.1.0
# This is a fork of FSharp.Formatting which parses .fs (not .fsx) files. 
# Compiled files are targetted to net461 and simply copied over to the applicable project.

rm -fr docs/doc-output && mkdir docs/doc-output

mono docs/doc-tools/fsformatting.exe \
    literate \
    --processDirectory \
    --inputDirectory "src" \
    --outputDirectory "docs/doc-output" \
    --templateFile "docs/doc-assets/templates/template.html"  \
    --replacements "page-author" " Your name" "project-name" "Learning F#"
