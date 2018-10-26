1. 

Data Flow Diagram
数据流图
活动图（activity diagram

EPC
PetriNet

2. 
-web
202.120.40.20:8821
downloader, reins
uploader, reins
-intro
202.120.40.28:61673
SE_public
-Python
ftp://public.sjtu.edu.cn
lucj/public
ftp://public.sjtu.edu.cn
litong.you python2018

3. 
g++ -std=c++1z
git clone / git pull
cd /...
git rm -r .
git add . 
git commit
git push origin master
svn co svn://ipads.se.sjtu.edu.cn/ics-se16/ics516030910005 icslabs --username=ics516030910005
 svn ci answer.txt --message="finish linux shell questions.”
curl -O 
npm -g install yarn

4. 
Command option etc 強制終了
sudo spring run .groovy
lsof -n -i4TCP:8080 | kill -9 
chmod -R 777 *
cd textbooks/tomcat/9.0.6/libexe/bin
./startup.sh
./shutdown
mysql.server start
mysql --user=root -p
source 
show databases;
drop database db_name
chrome://apps/

5. 

SPRINGSECURITY
https://github.com/rubytomato/demo-security-spring2

http://shinsuke789.hatenablog.jp/entry/2015/08/11/123000

https://qiita.com/kojisaiki/items/bfb5c023a80e0c75d8af

axios post url encoded
https://gist.github.com/akexorcist/ea93ee47d39cf94e77802bc39c46589b

6. 

Starting project
クリーンなdirecotryを作って始める！！
git clone -b branch_name http.git
cd SE_kuaijiebao ここ大事！！
“"
git fetch
git checkout remote_branch_nameでローカルfolderに落とす
“”””””””””””"
OR
“"
git pull origin updating_branch
“””””””””””””"

7.

- MERGE
git merge branch_merging_from

conflictの時は
git diff --name-only --diff-filter=U
conflictの解決
git checkout —thiers/ours file_name
でファイルごとに解決
最後に(conflictしてもしなくても)
git add. 
git commit -m “”
git push origin branch _name

8. 
- checkout branch
git checkout another_branchの時に
Please commit your changes or stash them before you switch branches.
Abortingの時は
git reset HEAD で unstage from cache

changes not staged for commitの時は
https://qiita.com/konweb/items/061475d6376db957b3c4

9. 
git config --global core.autoCRLF false

10. 
F1703701

git clone -b ブランチ名 https://リポジトリのアドレス

git log --pretty=format:"%h %s" —graphでコミットグラフ

11. 
strlen(“C”)==2 C\n
strlen(‘C’)==1 C

sudo docker run -it --privileged --cap-add=ALL -v /Users/aikawasatoshi/cselabs/lab-cse:/home/stu/devlop ddnirvana/cselab_env:latest /bin/bash
 file system identi er, an inode number, and a generation number
chinese
yacc
Our text book: Chap-5.2
3-easy-pieces: Chap-28279
315-
midterm lex yacc簡単な電卓紙に書くreg注意　C++実験復習 funcncall,stack

12. 
GRADLE
SPRING SECURITY
spring cloud
docker

groovy
http://groovyconsole.appspot.com/
GIT
https://www.packtpub.com/application-development/git-mastering-version-control
DOCKER https://www.packtpub.com/networking-and-servers/learn-docker-fundamentals-docker-18x