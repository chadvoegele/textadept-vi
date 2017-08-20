#!/usr/bin/env bash
# Thanks! https://github.com/schteppe/remote-physics/blob/master/agx/nodeSync.lua
# Thanks! http://www.linuxjournal.com/content/using-named-pipes-fifos-bash
BASE=$(readlink -f $(dirname $0))

TEST_DIR_BASE="/tmp/textadept-vi_testdir"
TEST_DIR=$(find /tmp -maxdepth 1 -path ${TEST_DIR_BASE}".*" -type d -user $USER | head -1)
if [ -z ${TEST_DIR} ]
then
  TEST_DIR=$(mktemp -d ${TEST_DIR_BASE}".XXXXXXXX")
  TA_MODULES="${TEST_DIR}/.textadept/modules"
  mkdir -p ${TA_MODULES}
  ln -s ${BASE} ${TA_MODULES}
  echo "Set up test directory ${TEST_DIR}"
else
  echo "Using existing test directory ${TEST_DIR}"
fi
SESSION_FILE="${TEST_DIR}/.textadept/session_term"
[ -f ${SESSION_FILE} ] && rm ${SESSION_FILE}

PIPE="/tmp/textadept-vi_testpipe"
[ ! -p ${PIPE} ] && mkfifo ${PIPE}

EXIT_TA() {
  TA_PID="${1}"
  TEST_OUTPUT_FILE=$(ls -t /tmp/lua_* | head -1)
  if [ ! -z ${TEST_OUTPUT_FILE} ]
  then
    TEST_OUTPUT=$(cat ${TEST_OUTPUT_FILE})
  fi
  kill ${TA_PID}
  sleep 0.1
  echo ${TEST_OUTPUT}
  [ -f ${TEST_OUTPUT_FILE} ] && rm ${TEST_OUTPUT_FILE}
  [ -p ${PIPE} ] && rm ${PIPE}
}

READ_PIPE() {
  while [ -p ${PIPE} ] && read LINE < ${PIPE}
  do
    EXIT_TA ${LINE}
  done
}

READ_PIPE &
READ_PIPE_PID="$!"

RUN_TESTS="require('textadept-vi.test').run()"
SEND_PID="local stat = io.open('/proc/self/stat', 'r') local pid = stat:read('*n') stat:close() io.open('"${PIPE}"', 'a'):write(tostring(pid)..'\n'):close()"
HOME=${TEST_DIR} textadept-curses -e "${RUN_TESTS} ${SEND_PID}"

wait ${READ_PIPE_PID}
