

if [ -z "${DISPLAY}" ] && [ "${XDG_VTNR}" -eq 2 ]; then
  exec sway
fi
