eca [![Build Status](https://travis-ci.org/yunnet/eca.svg?branch=master)](https://travis-ci.org/yunnet/eca)
=====

**An OTP application

##Overview
* 程序通过配置的侦听端口，终端连接此端口，上传原始数据；
* 通过协议分析出终端标识号码；
* 通过标识号码，匹配数据库的终端类型后，做进一步数据分析；
* 将数据写入到kafka.

##Build
    $ rebar3 compile

##monitor
![observer](https://github.com/yunnet/eca/blob/master/config/monitor.png "observer")  
