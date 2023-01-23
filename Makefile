SANDBOX_DIR?=sandbox
WORKSPACE_DIR=$(SANDBOX_DIR)/wsp

# prepare emacs sandbox with local library and dependencies installed
sandbox:
	./bin/makem.sh --sandbox=$(SANDBOX_DIR) --install-deps -vv

# run emacs in sandbox
.PHONY: emacs
emacs: sandbox
	./bin/makem.sh --sandbox=$(SANDBOX_DIR) interactive

.PHONY: clean-test
clean-test:
	rm -rf "${WORKSPACE_DIR}"

.PHONY=test
test: sandbox clean-test
	./bin/makem.sh --sandbox=$(SANDBOX_DIR) interactive -vv -- -l tests/wsp-test.el --eval "(wsp-test-workspace-create)"
	./bin/makem.sh --sandbox=$(SANDBOX_DIR) interactive -vv -- -l tests/wsp-test.el --eval "(wsp-test-workspace-restore1)"
	./bin/makem.sh --sandbox=$(SANDBOX_DIR) interactive -vv -- -l tests/wsp-test.el --eval "(wsp-test-workspace-restore2)"
	./bin/makem.sh --sandbox=$(SANDBOX_DIR) interactive -vv -- -l tests/wsp-test.el --eval "(wsp-test-project-find-file-should-start-at-project-root)"
	./bin/makem.sh --sandbox=$(SANDBOX_DIR) interactive -vv -- -l tests/wsp-test.el --eval "(wsp-test-workspace-with-multiple-projects-create)"
	./bin/makem.sh --sandbox=$(SANDBOX_DIR) interactive -vv -- -l tests/wsp-test.el --eval "(wsp-test-workspace-with-multiple-projects-restore)"
	./bin/makem.sh --sandbox=$(SANDBOX_DIR) interactive -vv -- -l tests/wsp-test.el --eval "(wsp-test-workspace-close)"
	./bin/makem.sh --sandbox=$(SANDBOX_DIR) interactive -vv -- -l tests/wsp-test.el --eval "(wsp-test-workspace-switch)"
	./bin/makem.sh --sandbox=$(SANDBOX_DIR) interactive -vv -- -l tests/wsp-test.el --eval "(wsp-test-workspace-delete)"
	./bin/makem.sh --sandbox=$(SANDBOX_DIR) interactive -vv -- -l tests/wsp-test.el --eval "(wsp-test-nested-project)"

clean:
	rm -rf $(SANDBOX_DIR)
