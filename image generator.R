# Install ollamar if not already installed
library(ollamar)
library(httr)    # for downloading the returned file
library(jsonlite)

resp <- generate(model = "llava",
                 prompt = "draw a circle in png",
                 output = "raw"
)

# Extract python code block from response
code <- sub("(?s).*```python\\n(.*)```.*", "\\1", resp, perl = TRUE)

# Write and run
writeLines(code, "draw.py")
system("python draw.py")
