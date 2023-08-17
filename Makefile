
.PHONY:default
default:
	quarto render

.PHONY:open
open:
	@open docs/index.html
