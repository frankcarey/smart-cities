pip install pip --upgrade
pip install virtualenv
virtualenv --distribute --no-site-packages .env
. .env/bin/activate
pip install -r requirements.txt

echo "======="
echo " run the following on the command line to activate the environment.:"
echo "    source .env/bin/activate"
echo "======="
