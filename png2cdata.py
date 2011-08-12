from PIL import Image

im = Image.open('wall.png')

bytes = []
for x in range(0,64):
	for yc in range(0,8):
		b = 0
		for y in range(0,8):
			pix = im.getpixel( (x, (yc<<3) | y) )
			if pix == (0,0,0):
				b = b | (1 << y)
		bytes.append(b)

print "const uint8_t wall[] = {%s};" % ','.join([str(n) for n in bytes])
