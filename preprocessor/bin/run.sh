#!/bin/bash

script_dir="$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")"
java -jar ${script_dir}/preprocessor.jar $@
