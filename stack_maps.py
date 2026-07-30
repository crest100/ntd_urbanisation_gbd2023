from PIL import Image
import numpy as np

top = Image.open("output/fig6_world_1990.png")
bot = Image.open("output/fig6_world_2023.png")

print(f"1990: {top.size}")
print(f"2023: {bot.size}")

# Paste top above bottom
w = max(top.width, bot.width)
h = top.height + bot.height
out = Image.new("RGB", (w, h), (255, 255, 255))
out.paste(top, (0, 0))
out.paste(bot, (0, top.height))
out.save("output/fig6_world_maps.png", dpi=(300, 300))

# Verify
arr = np.array(out.convert("RGB"))
h2, w2, _ = arr.shape
mid = h2 // 2
for name, chunk in [("1990", arr[:mid]), ("2023", arr[mid:])]:
    dark = (chunk.mean(axis=2) < 200).sum()
    total = chunk.shape[0] * chunk.shape[1]
    print(f"  {name}: {dark/total*100:.1f}% dark")

print(f"Combined: {out.size}")
