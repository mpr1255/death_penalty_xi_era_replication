﻿$(function(){var DefaultParam={appid:"",language:"",logData:[],BaseUrl:"https://t-cnki-net.virtual.anu.edu.au/collect",setInterval:false,intervalQueue:[],cookieTimeOut:60*24,tempData:[],};var ConfigParam={status:false,startTime:"",endTime:"",orginHosts:"",type:"mix",poolSize:"10",intervalTime:"10",custom:"",persist:false,};var LogParam={client:{appid:"","client-id":"","client-ip":"","user-id":"","user-name":"","user-organ":"",},data:{m:"",f:"",a:"",d:"",r:"",t:"",},};function init(data){if(!CANGJIE.DefaultParam.appid){console.error("未设定appid!")}var cangjieConfigParam=CANGJIE.getCookie("cangjieConfig_"+CANGJIE.DefaultParam.appid);if(cangjieConfigParam){CANGJIE.ConfigParam=JSON.parse(unescape(cangjieConfigParam));CANGJIE.doLog(data);return}CANGJIE.DefaultParam.tempData=new Array(data);var url=CANGJIE.DefaultParam.BaseUrl+"/ux-api/v1/app/profile";var headers={};headers.unitoken="";headers.uniplatform=CANGJIE.DefaultParam.appid;headers.language=CANGJIE.DefaultParam.language;var callback=function(data){if(data){if(data.data&&Object.keys(data.data).length>0){CANGJIE.ConfigParam.status=true;CANGJIE.ConfigParam.startTime=data.data["effected-date"];CANGJIE.ConfigParam.endTime=data.data["expired-date"];CANGJIE.ConfigParam.orginHosts=data.data["origin-hosts"];CANGJIE.ConfigParam.poolSize=data.data["pool-size"];CANGJIE.ConfigParam.intervalTime=data.data["interval-time"]?data.data["interval-time"]*1000:120*1000;CANGJIE.ConfigParam.custom=data.data["custom"];if(CANGJIE.ConfigParam.status){CANGJIE.setCookie("cangjieConfig_"+CANGJIE.DefaultParam.appid,JSON.stringify(CANGJIE.ConfigParam),CANGJIE.DefaultParam.cookieTimeOut)}for(var i=0;i<CANGJIE.DefaultParam.tempData.length;i++){CANGJIE.doLog(CANGJIE.DefaultParam.tempData[i])}}else{console.error("appid未注册:"+CANGJIE.DefaultParam.appid)}}};var rdata={appid:CANGJIE.DefaultParam.appid};CANGJIE.ajaxData(url,"GET",headers,rdata,callback)}function log(appid,data){if(!CANGJIE.DefaultParam.appid||CANGJIE.DefaultParam.appid!==appid){SendLogData();if(CANGJIE.DefaultParam.intervalQueue.length>0){for(var i=0;i<CANGJIE.DefaultParam.intervalQueue.length;i++){clearInterval(CANGJIE.DefaultParam.intervalQueue[i])}}CANGJIE.DefaultParam.appid=appid;CANGJIE.ConfigParam.status=false;CANGJIE.init(data);return}if(CANGJIE.ConfigParam.status===false){CANGJIE.DefaultParam.tempData.push(data);return}if(appid&&CANGJIE.DefaultParam.appid===appid){CANGJIE.doLog(data)}}function immediateLog(appid,data){log(appid,data);SendLogData()}function doLog(data){if(!CANGJIE.ConfigParam.status){return}var now=new Date();if(new Date(CANGJIE.ConfigParam.startTime+" 00:00:00")>now||new Date(CANGJIE.ConfigParam.endTime+" 23:59:59")<now){return}var logData={};if(data.c){data.a=data.c;delete data.c}$.extend(true,logData,data,{t:now.getTime()});CANGJIE.DefaultParam.logData.push(logData);typeStrategies[CANGJIE.ConfigParam.type]()}function clientData(){var loginStatus=CANGJIE.getCookie("Ecp_LoginStuts");var userId="";var userName="";var userOrgan="";if(loginStatus){userId=JSON.parse(loginStatus)["UserName"];userName=JSON.parse(loginStatus)["ShowName"];userOrgan=JSON.parse(loginStatus)["BUserName"]}return{"client-id":CANGJIE.getCookie("Ecp_ClientId"),"client-ip":CANGJIE.getCookie("Ecp_ClientIp"),"user-id":userId,"user-name":userName,"user-organ":userOrgan,}}var typeStrategies={immediately:function(){SendLogData()},count:function(){if(CANGJIE.DefaultParam.logData.length>=CANGJIE.ConfigParam.poolSize){SendLogData()}},time:function(){if(!CANGJIE.DefaultParam.setInterval){CANGJIE.DefaultParam.setInterval=true;var interval=setInterval(function(){SendLogData()},CANGJIE.ConfigParam.intervalTime);CANGJIE.DefaultParam.intervalQueue.push(interval)}},mix:function(){if(CANGJIE.DefaultParam.logData.length>=CANGJIE.ConfigParam.poolSize){SendLogData();return}if(!CANGJIE.DefaultParam.setInterval){CANGJIE.DefaultParam.setInterval=true;var interval=setInterval(function(){SendLogData()},CANGJIE.ConfigParam.intervalTime);CANGJIE.DefaultParam.intervalQueue.push(interval)}},};function SendLogData(){if(CANGJIE.DefaultParam.logData.length===0){return}var url=CANGJIE.DefaultParam.BaseUrl+"/ux-api/v1/app/batch-trace";var headers={};headers.unitoken="";headers.uniplatform=CANGJIE.DefaultParam.appid;headers.language=CANGJIE.DefaultParam.language;var logData=[];$.extend(true,logData,CANGJIE.DefaultParam.logData);CANGJIE.DefaultParam.logData=[];var data={data:logData,appId:CANGJIE.DefaultParam.appid,};var callback=function(){};CANGJIE.ajaxData(url,"POST",headers,data,callback,"application/json")}function ajaxData(url,type,headers,data,success,contentType){var params={url:url,type:type,headers:headers,data:data,success:success,error:function(data){console.error("error:"+data)},};if(contentType){params.contentType=contentType;params.data=JSON.stringify(params.data)}$.ajax(params)}function getCookie(cname){var name=cname+"=";var ca=document.cookie.split(";");for(var i=0;i<ca.length;i++){var c=ca[i].trim();if(c.indexOf(name)==0){return c.substring(name.length,c.length)}}return null}function setCookie(c_name,value,expiremMinutes){var exdate=new Date();exdate.setTime(exdate.getTime()+expiremMinutes*60*1000);document.cookie=c_name+"="+escape(value)+(expiremMinutes==null?"":";expires="+exdate.toGMTString())}var CANGJIE={};CANGJIE.DefaultParam=DefaultParam;CANGJIE.ConfigParam=ConfigParam;CANGJIE.LogParam=LogParam;CANGJIE.init=init;CANGJIE.log=log;CANGJIE.immediateLog=immediateLog;CANGJIE.doLog=doLog;CANGJIE.ajaxData=ajaxData;CANGJIE.clientData=clientData;CANGJIE.getCookie=getCookie;CANGJIE.setCookie=setCookie;window.CANGJIE=CANGJIE});