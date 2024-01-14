.PHONY: all tests st itests gt igt t i transform run irun

LGT=LOGTALKHOME=/usr/lib/logtalk/share/logtalk swilgt

LOADER=loader

run: transform

run:
	$(LGT) -g "{$(LOADER)},halt."

irun:
	$(LGT) -g "{$(LOADER)},logtalk_load(tools(loader))."
