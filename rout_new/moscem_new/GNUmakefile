#======================================================
# GNUmakefile
# requires Gnu Make
# recursively invokes make in the build directory
# Yuqiong Liu, March 2003 
#=====================================================

export TOP_DIR = $(shell pwd)
export CONFIG_FILE = $(TOP_DIR)/obj/config.in

BUILD_DIR1 = obj/moscem
BUILD_DIR2 = obj/postproc

include $(CONFIG_FILE)

MOSCEM: 
	cd $(BUILD_DIR1); $(MAKE) -f $(TOP_DIR)/moscem/GNUmakefile "MAKECMDGOALS = $@" $@

POSTPROC:
	cd $(BUILD_DIR2); $(MAKE) -f $(TOP_DIR)/postproc/GNUmakefile "MAKECMDGOALS = $@" $@


clean:
	rm -f  ../../core $(BUILD_DIR1)/*.[ocd]

clean_POSTPROC:
	rm -f  ../../core $(BUILD_DIR2)/*.[ocd]


.DEFAULT: 
	cd $(BUILD_DIR1); $(MAKE) -f $(TOP_DIR)/moscem/GNUmakefile "MAKECMDGOALS = $@" $@




