# 🎮 AI-Powered Text Adventure Game

An interactive, AI-driven text-based adventure game built with R Shiny and powered by local LLM models via Ollama. Create your own story sheets, choose your character, and let AI narrate your silly and creative adventures!

## ✨ Features

- 🎭 **Multiple Story Support**: Load different adventure scenarios from CSV files
- 🦁 **Character Selection**: Choose from various characters with unique stats (Strength, Stealth, Charisma, Cleverness)
- 🎨 **Character Sprites**: Display PNG sprites for each character
- 🤖 **AI-Powered Storytelling**: Uses local LLM models to generate creative success/failure narratives
- 🎲 **Dice Mechanics**: Success determined by idea quality + character skills + random luck
- 📊 **Progress Tracking**: See your journey through completed challenges
- 🔄 **Dynamic Context**: AI remembers your previous actions throughout the adventure

## 🎯 How It Works

1. **Choose Your Story**: Select from available adventure scenarios
2. **Pick Your Character**: Each character has different skill levels
3. **Face Challenges**: Read the challenge prompt
4. **Take Action**: Type what you want to do
5. **AI Evaluation**: The AI judges your idea (1-10 scale)
6. **Roll the Dice**: Your idea + character skill + luck vs. difficulty
7. **Get Results**: AI generates a creative success or failure story
8. **Progress**: Move to the next challenge or try again!

## 📋 Prerequisites

- R (>= 4.0.0)
- [Ollama](https://ollama.ai/) installed and running locally
- Required R packages:
  - `shiny`
  - `tidyverse`
  - `ollamar`

## 🚀 Installation

### 1. Install Ollama

```bash
# macOS/Linux
curl -fsSL https://ollama.com/install.sh | sh

# Windows: Download from https://ollama.com/download
```

### 2. Pull a Model

For modern computers:
```bash
ollama pull llama3.2
```

For older/slower computers (recommended for 2011-era hardware):
```bash
ollama pull phi3:mini
# or
ollama pull tinyllama
```

### 3. Clone This Repository

```bash
git clone https://github.com/yourusername/ai-adventure-game.git
cd ai-adventure-game
```

### 4. Install R Dependencies

```r
install.packages(c("shiny", "tidyverse", "ollamar"))
```

### 5. Configure Your Model

Edit `app.R` line 13:
```r
OLLAMA_MODEL <- "llama3.2"  # or "phi3:mini" or "tinyllama"
```

## 📁 Project Structure

```
ai-adventure-game/
├── app.R                    # Main Shiny application
├── data/                    # Story folders
│   └── Animal Heist/       # Example story
│       ├── Animal Heist - story sheet.csv
│       ├── Animal Heist - skill sheet.csv
│       ├── Lion.png
│       ├── Panda.png
│       ├── Snake.png
│       ├── Rabbit.png
│       ├── Unicorn.png
│       └── Cat.png
└── README.md
```

## 📝 Creating Your Own Stories

### Story Sheet Format (`[Story Name] - story sheet.csv`)

| Prompt | Goal | Difficulty | Relevant_skill |
|--------|------|------------|----------------|
| You need to sneak past the guard | Get past the guard without being seen | 18 | Stealth |
| The door is locked with a complex mechanism | Open the door | 20 | Cleverness |

### Skill Sheet Format (`[Story Name] - skill sheet.csv`)

| Character | Description | Strength | Stealth | Charisma | Cleverness |
|-----------|-------------|----------|---------|----------|------------|
| Lion | King of the beasts... | 5 | 1 | 4 | 1 |
| Snake | Most subtle of beasts... | 0 | 5 | 3 | 5 |

### Adding Character Sprites

Place PNG files named exactly as the character name in the story folder:
- `Lion.png`
- `Snake.png`
- etc.

Recommended size: 150x150 to 500x500 pixels

## 🎮 Running the Game

### Local Play

```r
# Start Ollama (in terminal)
ollama serve

# Run the Shiny app (in R)
shiny::runApp()
```

Then open your browser to: `http://localhost:3838`

## 🌐 Remote Access

> ⚠️ **Note**: Remote access deployment is currently in active development. The app works perfectly for local use, but deploying to cloud platforms like shinyapps.io requires additional configuration due to network restrictions with local Ollama instances.

### Current Status:
- ✅ **Local deployment**: Fully working
- 🚧 **Self-hosted Shiny Server**: In progress
- 🚧 **Cloud LLM integration**: In development
- 🚧 **shinyapps.io deployment**: Requires API changes

### Future Options:
- Self-hosted Shiny Server with local Ollama
- Integration with cloud LLM APIs (Anthropic Claude, Google Gemini)
- Docker containerization for easy deployment

## 🎨 Customization

### Adjust AI Temperature
In `app.R`, modify the `generate()` calls to add options:
```r
resp <- generate(OLLAMA_MODEL,
                 format = input_evaluation_format,
                 full_prompt,
                 output = "text",
                 options = list(temperature = 0.9))
```

### Change UI Colors
Edit the CSS in the `tags$style()` section of `app.R`

### Modify Difficulty Calculation
Edit line ~470 in `app.R`:
```r
success <- ((input_value + relevant_skill_level + random_value) >= current_story$Difficulty)
```

## 🐛 Troubleshooting

### "Connection refused" error
- Make sure Ollama is running: `ollama serve`
- Check Ollama is on the correct port (default: 11434)

### Slow responses
- Use a smaller model (phi3:mini or tinyllama)
- Close other applications
- Reduce max_tokens in generate() calls

### Model not found
- Pull the model: `ollama pull modelname`
- Check model name matches exactly in app.R

### Character sprites not showing
- Ensure PNG files are in the story folder
- File names must match character names exactly (case-sensitive)
- Check file permissions

## 🤝 Contributing

Contributions are welcome! Ideas for contributions:
- New story scenarios
- Character sprites
- UI improvements
- Cloud deployment solutions
- Additional game mechanics
- Multi-language support

## 📄 License

MIT License - feel free to use, modify, and distribute!

## 🙏 Acknowledgments

- Built with [R Shiny](https://shiny.rstudio.com/)
- Powered by [Ollama](https://ollama.ai/)
- Inspired by classic text adventure games
- Special thanks to the open-source LLM community

## 📧 Contact

Questions or suggestions? Open an issue or reach out!

---

**Enjoy your adventure!** 🗺️✨
