## Git и github:
Git - это система контроля версий. Она позволяет хранить несколько версий
программы. Это позволяет запоминать всю историю изменений и при необходимости
переходить на более старую или смешивать изменения нескольких людей в разных
версиях на разных машинах.
На системах Unix, в том числе и Fedora, git установлен изначанльно.

Github - это web-версия системы git, а также хранилеще проетов, чтобы копировать
проекты не напрямую с машины одного разработчика на мащину другого, но
копировать через github.

Чтобы иметь возможность скачать с github чей либо проект, включая наш, необходимо
зарегестрироваться на сайте https://github.com.

После регестрации можно скопировать программу себе на компьютер с помощью
команды git clone:
```
git clone https://github.com/DzyubSpirit/HaskMPI
```
В текущей директории создастся папка HaskMPI с всеми файлами проекта. Чтобы
зайти в папку можно воспольззоватся командой cd:
```
cd HaskMPI
```
Чтобы посмотреть список файлов нужна команда ls:
```
ls
```

### Создание репозитория

Чтобы создать репозиторий на сайте [github](https://github.com) вверху справа необходимо нажать плюсик и выбрать пункт "New repository" либо перейти по [ссылке](https://github.com/new).
Далее заполнить имя репозитория, все остальные параметры необязяательны. И нажать кнопку "Create repository".
Дальше появится описание того, как можно скачать репозиторий себе на компьютер, а также добавить файлы, закоммитить их и запушить обратно на гитхаб.

## ghc и stack:

ghc(Glasgow Haskell Compiler) - компилятор Haskell.
stack - пакетный менеджер для Haskell.

stack сам устанавливает ghc, поэтому нужно установить только stack такой
командой:
```
curl -sSL https://get.haskellstack.org/ | sh
stack install cabal-install
```

Теперь можно скомпилировать программму. Нужно переместится в папку проекта,
настроить stack и собрать проект:
```
cd HaskMPI
stack setup
stack build
```

Если сборка не прошла успешно, то это возможно из-за остутсвия c-библиотек (mpi,
open-rte, open-pal). Чтобы установить их в Fedora нужно выполнить команду:
```
sudo dnf install openmpi openmpi-devel
```
Затем попробовать собрать еще раз:
```
stack build
```

Чтобы запустить программу необходимо сперва настроить mpi:
Сначала создать ссылку на mpirun, чтобы можно было вместо
/usr/lib64/openmpi/bin/mpirun писать просто mpirun.
```
sudo ln -s /usr/lib64/openmpi/bin/mpirun /usr/bin
```
Затем добавить путь к библиотекам openmpi для всех приложений. Для этого нужно
создать файл /etc/ld.so.conf.d/openmpi-x86_64.conf и записать в нем путь к
файлам библиотеки /usr/lib64/openmpi/lib.
Чтобы сделать через консоль можно использовать такие команды:
```
sudo vim /etc/ld.so.conf.d/openmpi-x86_64.conf
```
Затем нажать 'i' ввести /usr/lib64/openmpi/lib, нажать ESC. Чтобы выйти нужно
написать :wq.
Чтобы изменения вступили в силу нужно выполнить команду:
```
sudo ldconfig
```

Наконец можно запустить программу:
```
stack exec -- mpirun HaskMPI-exe -np 4
```
