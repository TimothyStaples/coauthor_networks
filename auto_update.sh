message="auto-commit from on $(date)"
GIT=`which git`
REPO_DIR=~/Dropbox/Tim/CV/collabNetwork
cd ${REPO_DIR}
${GIT} add --all .
${GIT} commit -m "$message"
${GIT} push git@github.com:TimothyStaples/coauthor_networks

echo "$gitPush"


