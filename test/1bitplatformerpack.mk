#GET=curl -s
GET=wget
all: 1bitplatformerpack.png 1bitplatformerpack.bmp 1bitplatformerpack.txt
1bitplatformerpack.png:
	$(GET) http://decapode314.free.fr/games/platform-1b/img/1bitplatformerpack.png
1bitplatformerpack.bmp: 1bitplatformerpack.png
	convert $< $@
1bitplatformerpack.txt:
	echo '' > $@
	echo 'https://opengameart.org/content/1-bit-platformer-pack' >> $@
	echo 'Graphics Author: https://opengameart.org/users/kenney' >> $@
	echo 'License: CC0' >> $@
clean:
	$(RM) 1bitplatformerpack.png
	$(RM) 1bitplatformerpack.bmp
	$(RM) 1bitplatformerpack.txt
