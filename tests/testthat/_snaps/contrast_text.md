# Contrasting works

    Code
      contrast_text(colors)
    Output
      [1] "black" "white" "white" "white" "black" "white"

---

    Code
      contrast_text(colors, light_text = "blue", dark_text = "grey10", method = "relative",
        threshold = 0.1)
    Output
      [1] "grey10" "blue"   "grey10" "blue"   "grey10" "grey10"

---

    Code
      contrast_text(colors, light_text = "blue", dark_text = "grey10", method = "perceived",
        threshold = 0.7)
    Output
      [1] "grey10" "blue"   "blue"   "blue"   "grey10" "blue"  

