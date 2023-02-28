-include lib/borg/borg.mk

ifndef BORG_DIR

help helpall::
	$(info )
	$(info Bootstrapping)
	$(info -------------)
	$(info make bootstrap-borg  = make borg and make targets available)
	@printf "\n"

bootstrap-borg:
	@mkdir .git/modules
	@git clone https://github.com/emacscollective/borg lib/borg \
	--separate-git-dir .git/modules/borg
	@cd lib/borg; git symbolic-ref HEAD refs/heads/main
	@cd lib/borg; git reset --hard HEAD

endif
