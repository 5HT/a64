default:
	echo "-define(VERSION,\"`git describe --tags`\")." > include/a64.hrl
	mad cle dep com str bun a64
