# MindNotes

## Setup systemctl

* deploy:

  ```bash
  ./build.sh
  ```

* copy to `/opt`:

  ```bash
  sudo mkdir -p /opt/mindnotes
  sudo cp -r deploy/* /opt/mindnotes
  ```

* добавить сервис:

  > [!NOTE]
  > `DOTNET_ROOT=$HOME/.dotnet` — место установки `dotnet`. Если у вас нормально установлен он, тогда вам это не надо.

  ```bash
  sudo bash -c "cat > /usr/lib/systemd/system/mindnotes.service << EOF
  [Unit]
  Description=Run mindnotes
  After=default.target

  [Service]
  ExecStart=/usr/bin/env -S bash -c \"cd /opt/mindnotes && DOTNET_ROOT=$HOME/.dotnet DOTNET_CLI_TELEMETRY_OPTOUT=true DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=1 ./Server\"
  Restart=on-failure
  RestartSec=1s
  Type=simple

  [Install]
  WantedBy=default.target
  EOF"
  ```

* Запустить:

  ```bash
  sudo systemctl daemon-reload
  sudo systemctl start mindnotes.service
  ```

* Проверить, что всё хорошо:

  ```bash
  systemctl status mindnotes.service
  ```

  Если что, можно запустить вот это:

  ```bash
  journalctl -u mindnotes.service
  ```

* Запускать вместе с ОСью:

  ```bash
  sudo systemctl enable mindnotes.service
  ```
