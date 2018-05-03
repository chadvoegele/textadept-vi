#!/usr/bin/env bash
# Thanks! https://github.com/schteppe/remote-physics/blob/master/agx/nodeSync.lua
# Thanks! http://www.linuxjournal.com/content/using-named-pipes-fifos-bash
BASE=$(readlink -f $(dirname $0))

TEST_DIR=$(mktemp -d /tmp/textadept-vi_testdir.XXXXXXXX)
TA_MODULES="${TEST_DIR}/.textadept/modules"
mkdir -p ${TA_MODULES}
ln -s ${BASE} ${TA_MODULES}
echo "Set up test directory ${TEST_DIR}"

TEST_OUTPUT_FILE=$(mktemp)

PIPE="/tmp/textadept-vi_testpipe"
[ ! -p ${PIPE} ] && mkfifo ${PIPE}

EXIT_TA() {
  TA_PID="${1}"
  kill ${TA_PID}
  sleep 0.1
  cat ${TEST_OUTPUT_FILE}
  [ -p ${PIPE} ] && rm ${PIPE}
  [ -f ${TEST_OUTPUT_FILE} ] && rm ${TEST_OUTPUT_FILE}
  [ -d ${TEST_DIR} ] && rm -r ${TEST_DIR}
}

READ_PIPE() {
  while [ -p ${PIPE} ] && read LINE < ${PIPE}
  do
    EXIT_TA ${LINE}
  done
}

READ_PIPE &
READ_PIPE_PID="$!"

RUN_TESTS="require('textadept-vi.test').run('${TEST_OUTPUT_FILE}')"
SEND_PID="local stat = io.open('/proc/self/stat', 'r') local pid = stat:read('*n') stat:close() io.open('"${PIPE}"', 'a'):write(tostring(pid)..'\n'):close()"
HOME=${TEST_DIR} textadept-curses -e "${RUN_TESTS} ${SEND_PID}"

wait ${READ_PIPE_PID}
