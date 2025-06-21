# Locations
OUTPUT_DIR := bin
NAME := teaspoon

# Tools
RM := rm -rf

# The make rules
all: build

# Builds the project, along with all of the dependencies, into a single jar
build:
	sbt assembly

clean:
	sbt clean
	$(RM) target $(OUTPUT_DIR)

install:
	mkdir -p /opt/$(NAME)/
	cp -r $(OUTPUT_DIR)/* /opt/$(NAME)/
	ln -sf /opt/$(NAME)/run.sh /usr/bin/$(NAME)

.PHONY: all clean