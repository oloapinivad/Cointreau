# declare a few folders
DIR="/home/paolo/covid"
ITADIR=$DIR/COVID-ITALY
WORDIR=$DIR/COVID-WORLD
WWWDIR="/var/www/html/diss/paolo/covid-19"
FIGDIR="/work/users/paolo/figures/COVID-19"

# fetch of italian and world data
cd $ITADIR
git fetch
git pull

cd $WORDIR
git fetch
git pull

# run the script
/usr/bin/Rscript $DIR/Cointreau/covid.R

# create figure names and copy to the webserver
filename="covid-status-$(date +%Y-%m-%d).svg"
today="covid-status-today.svg"
echo $outname

cp $FIGDIR/$filename $WWWDIR
ln -sf $WWWDIR/$filename $WWWDIR/$today
cp $DIR/Cointreau/index.html $WWWDIR
