SANDBOX_DIR?=sandbox
WORKSPACE_DIR=$(SANDBOX_DIR)/wsp

.PHONE=test
test: clean
	rm -rf $(WORKSPACE_DIR)
	./bin/makem.sh --sandbox=$(SANDBOX_DIR) --install-deps interactive -vv -- -l tests/wsp-test.el --eval "(wsp-test-workspace-create)"
	./bin/makem.sh --sandbox=$(SANDBOX_DIR) interactive -vv -- -l tests/wsp-test.el --eval "(wsp-test-workspace-restore1)"
	./bin/makem.sh --sandbox=$(SANDBOX_DIR) interactive -vv -- -l tests/wsp-test.el --eval "(wsp-test-workspace-restore2)"
	./bin/makem.sh --sandbox=$(SANDBOX_DIR) interactive -vv -- -l tests/wsp-test.el --eval "(wsp-test-workspace-close)"
	./bin/makem.sh --sandbox=$(SANDBOX_DIR) interactive -vv -- -l tests/wsp-test.el --eval "(wsp-test-workspace-switch)"
	./bin/makem.sh --sandbox=$(SANDBOX_DIR) interactive -vv -- -l tests/wsp-test.el --eval "(wsp-test-workspace-delete)"
	./bin/makem.sh --sandbox=$(SANDBOX_DIR) interactive -vv -- -l tests/wsp-test.el --eval "(wsp-test-new-workspace-with-multiple-projects)"
	./bin/makem.sh --sandbox=$(SANDBOX_DIR) interactive -vv -- -l tests/wsp-test.el --eval "(wsp-test-new-workspace-with-multiple-projects-restore)"


clean:
	rm -rf $(SANDBOX_DIR)
