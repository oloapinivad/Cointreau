# declare a few folders
wave=$1

DIR="/home/paolo/covid"
ITADIR=$DIR/COVID-ITALY
WORDIR=$DIR/COVID-WORLD
ENGDIR=$DIR/COVID-UK
WWWDIR="/var/www/html/diss/paolo/covid-19/wave$wave"
mkdir -p $WWWDIR
FIGDIR="/work/users/paolo/figures/COVID-19/$wave"

# fetch of italian and world data
for dir in $ITADIR $WORDIR $ENGDIR ; do
	cd $dir
	git fetch
	git pull
done

# run the script
/usr/bin/Rscript $DIR/Cointreau/covid.R

# create figure names and copy to the webserver
filename="covid-status-$(date +%Y-%m-%d).svg"
today="covid-status-today.svg"
echo $outname

# copy figures and link
cp $FIGDIR/$filename $WWWDIR
ln -sf $WWWDIR/$filename $WWWDIR/$today

# prepare html
cp $DIR/Cointreau/index_wave$wave.tmpl $WWWDIR/index.html
sed -i "s/<DATE>/$(date)/g" "$WWWDIR/index.html"

for i in $(seq -10 -1);
do
	ll=$(date -d "($date) $i days" +%Y-%m-%d);
	if [ -f $FIGDIR/covid-status-$ll.svg ] ; then
		echo $ll
		cp $FIGDIR/covid-status-$ll.svg $WWWDIR
		insert="<br/> <font size="3" > Forecasts at <a href=http://wilma.to.isac.cnr.it/diss/paolo/covid-19/wave$wave/covid-status-$ll.svg > $ll </a></i> </font>"
		sed "/past 10 days/a ${insert}" -i "$WWWDIR/index.html"
	fi
done;

# italian gif
convert -delay 100 -loop 0 $FIGDIR/forecast/italy/*.pdf $WWWDIR/italyGIF.gif

# prediction
cp $FIGDIR/forecast*evolution.svg $WWWDIR

# england prediction
#cp $FIGDIR/forecast/england/*.pdf $WWWDIR/england
