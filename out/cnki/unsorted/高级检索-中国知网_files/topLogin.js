﻿jQuery.fn.shake = function (times, offset, delay) {
    this.stop().each(function () {
        var Obj = $(this);
        var marginLeft = parseInt(Obj.css('margin-left'));
        Obj.animate({ 'margin-left': marginLeft + offset }, delay, function () {
            Obj.animate({ 'margin-left': marginLeft }, delay, function () {
                times = times - 1;
                if (times > 0)
                    Obj.shake(times, offset, delay);
            });
        });
    });
    return this;
};
function shake2(Obj, times, offset, delay) {
    Obj.stop().each(function () {
        var marginLeft = parseInt(Obj.css('margin-left'));
        Obj.animate({ 'margin-left': marginLeft + offset }, delay, function () {
            Obj.animate({ 'margin-left': marginLeft }, delay, function () {
                times = times - 1;
                if (times > 0)
                    shake2(Obj, times, offset, delay);
            });
        });
    });
}

function getSubCookie(coo, key) {
    if (coo !== undefined && key !== undefined) {
        var subCoo = coo.split("&");
        for (var i = 0; i < subCoo.length; i++) {
            var index = subCoo[i].indexOf("=");
            var sub = subCoo[i].substring(0, index);
            if (decodeURIComponent(sub) === key)
                return decodeURIComponent(subCoo[i].substring(index + 1));
        }
    }
    return "";
}

function Ecp_ShowMsgFocus(msg, focusEle) {
    Ecp_ShowMsg(msg);

    if (focusEle)
        focusEle.focus();
}
function Ecp_ShowMsgShake(msg, noshake) {
    Ecp_ShowMsg(msg);

    if (!noshake)
        shake2($('#Ecp_top_login_layer'), 3, 4, 80);
    //$('#Ecp_top_login_layer').shake(3, 4, 80);
}

function Ecp_ShowMsg(msg) {
    if (msg && msg.length > 0) {
        $('#Ecp_errorMsg').text(msg).show();
        $(".login-title").css("margin-bottom", 0);
    }
    else {
        $('#Ecp_errorMsg').text("").hide();
        $(".login-title").css("margin-bottom", "24px");
    }
}

Date.prototype.Format = function (fmt) {
    var o = {
        "M+": this.getMonth() + 1,  //月份   
        "d+": this.getDate(),
        "H+": this.getHours(),
        "m+": this.getMinutes(),
        "s+": this.getSeconds(),
        "q+": Math.floor((this.getMonth() + 3) / 3),//季度   
        "S": this.getMilliseconds() //毫秒   
    };
    if (/(y+)/.test(fmt))
        fmt = fmt.replace(RegExp.$1, (this.getFullYear() + "").substr(4 - RegExp.$1.length));
    for (var k in o)
        if (new RegExp("(" + k + ")").test(fmt))
            fmt = fmt.replace(RegExp.$1, RegExp.$1.length === 1 ? o[k] : ("00" + o[k]).substr(("" + o[k]).length));
    return fmt;
};

var Ecp_IsShowCheck;
var Ecp_LoginStuts = "Ecp_LoginStuts";
var Ecp_notFirstLogin = "Ecp_notFirstLogin";
var b_AutoLogin;
var b_newLogin = false;
var Ecp_ResultR;
var Ecp_IsLogin = false;
var Ecp_CookieOtherDomain = '';

function FlushLogin() {
    $("#Ecp_top_login_closeLayer").unbind("click").bind("click", function () { Ecp_CloseLayer(); });
    $("#Ecp_CheckCodeImg").unbind("click").bind("click", function () { Ecp_ReGetImg(); });
    $("#Ecp_CheckLink").unbind("click").bind("click", function () { Ecp_ReGetImg(); });
    $("#Ecp_Button1").unbind("click").bind("click", function () { return Ecp_SubmitCheck(1, this, false); });
    $("#Ecp_Button2").unbind("click").bind("click", function () { return Ecp_IpLogin(true); });

    Ecp_ValdateInput("Ecp_TextBoxUserName");
    Ecp_ValdateInput("Ecp_TextBoxPwd");
    Ecp_ValdateInput("Ecp_CheckCode");

    $("#Ecp_top_login").unbind("click").bind("click", function () { Ecp_ShowLoginLayer2('-90px', '42px'); });
    $("#Ecp_top_logout_showLayer").unbind("click").bind("click", function () { Ecp_ShowLogOutLayer(); });
    $("#Ecp_top_logoutClick").unbind("click").bind("click", function () { Ecp_LogoutOptr_my(); });

    $("#ecpover_open").unbind("click");

    var domain1 = document.domain;
    if (domain1.toLowerCase().indexOf('cnki.net', domain1.toLowerCase().length - 'cnki.net'.length) == -1) {
        var intd = domain1.indexOf('cnki.net');
        if (intd > 0) {
            Ecp_CookieOtherDomain = domain1.substr(intd);
        }
        else {
			Ecp_CookieOtherDomain = domain1.replace('www.', '');
        }
    }

    //if (Ecp_CookieOtherDomain !== '')
    //    Ecp_topLoginUrl = 'http://o.' + Ecp_CookieOtherDomain + '/TopLoginI18N/';
    //else
//if(domain1.indexOf("libezproxy.must.edu.mo")>0)
//        Ecp_topLoginUrl = '//o-cnki-net.libezproxy.must.edu.mo/TopLoginI18N/';	
//else
        Ecp_topLoginUrl = 'https://o-cnki-net.virtual.anu.edu.au/TopLoginI18N/';

    //$(function () { $('input, textarea').placeholder(); });

    var uid = Ecp_GetQueryString("uid");
    if (uid && uid.length > 100) {
        var cuid5 = cookie('ecp_uid5');
        var uid5 = $.md5(uid);
        if (cuid5 === undefined || cuid5 && cuid5.length >= 16 && cuid5 !== uid5) {
            cookie("ecp_uid5", uid5);
            var autoLogin = Ecp_GetQueryString("autoLogin");
            if (autoLogin && autoLogin === '1')
                b_AutoLogin = true;

            b_newLogin = true;
            Ecp_UidLogin(uid);
            return;
        }
    }
    Ecp_FlushLogin();
}

function Ecp_FlushLogin() {
    var v = cookie(Ecp_LoginStuts);
    if (v && v.length > 0) {
        var vj = JSON.parse(v);

        var ses = cookie("Ecp_session");
        if (!ses || ses.length === 0 && ses === "") {
            b_newLogin = true;
        }

        var nf = cookie(Ecp_notFirstLogin);
        if (nf && nf.length > 0) {
            if (nf !== vj.r) {
                b_newLogin = true;
            }
        } else {
            Ecp_ResultR = vj.r;
            b_newLogin = true;
        }

        if (b_newLogin) {
            var uid = cookie('c_m_LinID');
            if (uid !== undefined && uid !== null) {
                uid = getSubCookie(decodeURIComponent(uid), "LinID");
            }
            if (uid !== undefined && uid !== null)
                Ecp_UidLogin(uid);
            else
                Ecp_UidLogin();
        } else {
            //console.log("Top:Ecp_IpLoginResult," + vj.ShowName + "|" + vj.r);
            Ecp_LoginResult(vj);
        }
    }
    else if (Ecp_isAuotIpLogin === '1') {
        var faultIp = cookie("Ecp_IpLoginFail");
        var isLogout = cookie("Ecp_lout");

        if ((!faultIp || faultIp.length <= 0) && (!isLogout || isLogout.length <= 0)) {
            $.ajax({
                url: Ecp_topLoginUrl + "api/loginapi/IpLoginFlush",
                dataType: "jsonp",
                success: function (result) {
                    //console.log("Top:IpLoginFlush," + result.IsSuccess + "|" + result.ErrorCode + "|" + result.ErrorMsg);
                    if (result.success && Ecp_CookieOtherDomain.length > 0) {
                        cookie("Ecp_IpLoginFail", "", { expires: -1, path: '/', domain: Ecp_CookieOtherDomain }, false);
                    }
                    Ecp_LoginSuccessOne(result);
                }
            });
        }
    }
}

function Ecp_ShowLogOutLayer() {
    $('#Ecp_top_login_layer').hide();
    $('#Ecp_top_logout_layer').toggle();
}

function Ecp_CloseLayer() {
    if (Ecp_Style === '2') {
        $("#Ecp_modal_he").hide();
        $("#Ecp_shadow_he").hide();
        $("#Ecp_top_login_layer").hide();
    }
    else {
        $('#Ecp_top_login_layer').hide(500);
    }
}

function Ecp_ShowLoginLayer2(left, top) {
    Ecp_ShowMsg();
    if (Ecp_Style === '2') {
        $("#Ecp_modal_he").show();
        $("#Ecp_shadow_he").show();
        $("#Ecp_top_login_layer").show();
        $('#Ecp_top_login_layer').animate({ "top": "50%", "left": "50%", "margin-top": "-173px" }, 800);
    }
    else {
        if (left && top)
            $('#Ecp_top_login_layer').css({ 'left': left, 'top': top }).toggle();
        else
            $('#Ecp_top_login_layer').toggle();
    }
}


//login----------------
function Ecp_UserLogin(userName, pwd) {
    b_AutoLogin = $("#rememberMe").prop("checked");
    var checkCode = '';
    if (Ecp_IsShowCheck) {
        checkCode = $("#Ecp_CheckCode");
        var ccode = checkCode.val();
        if (ccode === '') {
            Ecp_ShowMsgFocus(getLoginResource("NeedCode"), checkCode);//'请输入的验证码'
            return false;
        }
        if (RegexCheck(/^[0-9]{4,4}$/, ccode) === false) {
            Ecp_ShowMsgFocus(getLoginResource("NeedRightCode"), checkCode);//'请输入正确的验证码'
            return false;
        }
    }
    Ecp_ShowMsg(getLoginResource("Logining"));//"正在登录......"
    var param;
    if (Ecp_IsShowCheck) {
        param = {
            userName: userName, pwd: pwd, isAutoLogin: b_AutoLogin, checkCode: checkCode.val()
        };
    }
    else {
        param = {
            userName: userName, pwd: pwd, isAutoLogin: b_AutoLogin
        };
    }

    $.ajax({
        url: Ecp_topLoginUrl + "api/loginapi/Login" + "?lang=" + lang,
        data: param,
        cache: false,
        dataType: "jsonp",
        success: function (result) {
            //console.log("Top:UserLogin," + result.IsSuccess + "|" + result.ErrorCode + "|" + result.ErrorMsg);
            Ecp_LoginSuccessOne(result);
        },
        error: function (XMLHttpRequest, textStatus, errorThrown) {
            //alert(getLoginResource("LoginError") + XMLHttpRequest.status + XMLHttpRequest.statusText + " " + XMLHttpRequest.readyState + " " + textStatus);//"登录出错:"
        }
    });
}

function Ecp_IpLogin(forceLogin) {
    b_AutoLogin = $("#rememberMe").prop("checked");
    var checkCode = '';
    if (Ecp_IsShowCheck) {
        checkCode = $("#Ecp_CheckCode");
        var ccode = checkCode.val();
        if (ccode === '') {
            Ecp_ShowMsgFocus(getLoginResource("NeedCode"), checkCode);//'请输入的验证码'
            return false;
        }
        if (RegexCheck(/^[0-9]{4,4}$/, ccode) === false) {
            Ecp_ShowMsgFocus(getLoginResource("NeedRightCode"), checkCode); //'请输入正确的验证码'
            return false;
        }
    }

    Ecp_ShowMsg(getLoginResource("Logining"));//"正在登录......"

    if (forceLogin)
        forceLogin = true;
    else
        forceLogin = false;

    var cCode;
    if (Ecp_IsShowCheck) {
        cCode = checkCode.val();
    }
    else {
        cCode = "";
    }

    var param;
    param = {
        isAutoLogin: b_AutoLogin, checkCode: cCode, isForceLogin: forceLogin
    };

    $.ajax({
        url: Ecp_topLoginUrl + "api/loginapi/IpLogin" + "?lang=" + lang,
        data: param,
        dataType: "jsonp",
        success: function (result) {
            //console.log("Top:IpLogin," + result.IsSuccess + "|" + result.ErrorCode + "|" + result.ErrorMsg);
            if (result.success && Ecp_CookieOtherDomain.length > 0) {
                cookie("Ecp_IpLoginFail", "", { expires: -1, path: '/', domain: Ecp_CookieOtherDomain }, false);
            }
            Ecp_LoginSuccessOne(result);
        }
    });
}

function Ecp_LoginSuccessOne(result) {
    if (result.IsSuccess === true) {
        b_newLogin = true;
        Ecp_IsShowCheck = false;
        $("#Ecp_CheckCodeImg").attr({ src: "" });
        $("#Ecp_CheckCodeLayer").hide();
    }
    Ecp_LoginResult(result);
}

function Ecp_UidLogin(uid) {
    var url1 = Ecp_topLoginUrl + "api/loginapi/UidLogin";
    if (uid && uid.length > 0)
        url1 += "?uid=" + uid + "&cookieLogin=true";
    $.ajax({
        url: url1,
        dataType: "jsonp",
        success: function (result) {
            //console.log("Top:UidLogin," + result.IsSuccess + "|" + result.ErrorCode + "|" + result.ErrorMsg);
            if (!result.IsSuccess) {
                var uid = cookie('c_m_LinID');
                var u = "";
                if (uid !== null) {
                    u = getSubCookie(decodeURIComponent(uid), "LinID");
                }
                //console.log("Top:UidLogin," + u);
            }
            if (result.IsSuccess === true) {
                //Ecp_ResultR = result.r;
                Ecp_LoginResult(result);
                return;
            }
            else {
                Ecp_loginFalse();
                Ecp_ReomveCookie();
                Ecp_ShowMsgShake(result.ErrorMsg);
            }
        }
    });
}

function Ecp_loginFalse() {
    Ecp_IsLogin = false;
    Ecp_ShowMsg();
    $('#Ecp_top_login_oversea').hide();
}

function Ecp_ReomveCookie() {
    cookie(Ecp_LoginStuts, "", { expires: -1, path: '/', domain: Ecp_CookieDomain }, false);
    cookie(Ecp_notFirstLogin, "", { expires: -1 }, false);
    cookie("c_m_expire", "", { expires: -1, path: '/', domain: Ecp_CookieDomain }, false);
    cookie("c_m_LinID", "", { expires: -1, path: '/', domain: Ecp_CookieDomain }, false);
    cookie("Ecp_session", "", { expires: -1 }, false);
    if (Ecp_CookieOtherDomain.length > 0) {
        cookie(Ecp_LoginStuts, "", { expires: -1, path: '/', domain: Ecp_CookieOtherDomain }, false);
        cookie("c_m_expire", "", { expires: -1, path: '/', domain: Ecp_CookieOtherDomain }, false);
        cookie("c_m_LinID", "", { expires: -1, path: '/', domain: Ecp_CookieOtherDomain }, false);
        cookie("Ecp_session", "", { expires: -1, path: '/', domain: Ecp_CookieOtherDomain }, false);
    }
}

function Ecp_UserLogout() {
    Ecp_loginFalse();

    $("#Ecp_TextBoxUserName").val("");
    $("#Ecp_TextBoxPwd").val("");
    $("#Ecp_CheckCode").val("");
    $("#rememberMe").attr("checked", false);

    var url1 = Ecp_topLoginUrl + "api/loginapi/Logout";

    $.ajax({
        url: url1,
        dataType: "jsonp",
        cache: false,
        async: false,
        success: function (result) {
            var r = (new Date()).Format("HH:mm:ss");
            r = result.Msg + " " + r;
            $("#Ecp_LoginUid").val(r);
        },
        error: function (XMLHttpRequest, textStatus, errorThrown) {
            $("#Ecp_LoginUid").val(textStatus + ":" + errorThrown);
        },
        complete: function (XMLHttpRequest, textStatus) {
            cookie("Ecp_lout", 1, { path: '/', domain: Ecp_CookieDomain }, false);
            if (Ecp_CookieOtherDomain.length > 0) {
                cookie("Ecp_lout", 1, { path: '/', domain: Ecp_CookieOtherDomain }, false);
            }
            Ecp_ReomveCookie();
        }
    });

    if (typeof lead_remove === 'function')
        lead_remove();
}


function Ecp_LoginResult(vj) {
    if (vj === undefined || vj === null) {
        Ecp_loginFalse();
        Ecp_ReomveCookie();
        Ecp_ShowMsgShake(getLoginResource("LoginFailed"));//"登录失败"
        return;
    }

    if (vj.IsShowCheck) {
        $("#Ecp_CheckCode").val("");
        Ecp_IsShowCheck = true;
        $("#Ecp_CheckCodeImg").attr({ src: Ecp_topLoginUrl + "api/loginapi/Ecp_CheckCode" });
        $("#Ecp_CheckLink").click();
        $("#Ecp_CheckCodeLayer").show();
    }
    else {
        Ecp_IsShowCheck = false;
        $("#Ecp_CheckCodeImg").attr({ src: "" });
        $("#Ecp_CheckCodeLayer").hide();
    }

    //oversea-----------------
    if (vj.Msg === 'showoversea') {
        if (vj.ErrorMsg === null || vj.ErrorMsg === undefined || vj.ErrorMsg === '') {
            Ecp_loginFalse();
            Ecp_ReomveCookie();
            Ecp_ShowMsgShake(getLoginResource("LoginFailed"));//"登录失败"
            return;
        }

        $('#Ecp_top_login_oversea').show();

        if (vj.ErrorCode === -1 || vj.ErrorCode === -2) {
            $('#ecpover_select').hide();
            $('#ecpover_close').show();
            $("#ecpover_p_close").html(vj.ErrorMsg);
            Ecp_ShowMsgShake(getLoginResource("LoginFailed"), true);//"登录失败"
        }
        else {
            $('#ecpover_select').show();
            $('#ecpover_close').hide();

            $("#ecp_over_i").text(vj.ErrorCode);
            $("#ecpover_open").unbind("click").bind("click", function () {
                $.ajax({
                    url: Ecp_topLoginUrl + "api/loginapi/OverSeaOpen?k=" + vj.ErrorMsg,
                    dataType: "jsonp",
                    success: function (result) { Ecp_LoginResult(result); }
                });
            });
        }
        return;
    }
    else {
        $('#Ecp_top_login_oversea').hide();
    }
    //oversea-----------------

    if (vj.ShowName == null || vj.ShowName == undefined || vj.ShowName == '' || vj.success == false) {
        Ecp_loginFalse();
        Ecp_ReomveCookie();
        if (vj.ErrorMsg === null || vj.ErrorMsg === undefined || vj.ErrorMsg === '')
            Ecp_ShowMsgShake(getLoginResource("LoginFailed"));//"登录失败"
        else {//            "IP自动登录失败"
            if (vj.ErrorMsg === getLoginResource("IPLoginFailed")) {
                if (vj.r !== null) {
                    var d2 = new Date();
                    d2.setHours(d2.getHours() + 24);
                    cookie("Ecp_IpLoginFail", vj.r, { expires: d2, path: '/', domain: Ecp_CookieDomain }, false);
                    if (Ecp_CookieOtherDomain.length > 0) {
                        cookie("Ecp_IpLoginFail", vj.r, { expires: d2, path: '/', domain: Ecp_CookieOtherDomain }, false);
                    }
                }
                //Ecp_isAuotIpLogin = "0";
            }
            Ecp_ShowMsgShake(vj.ErrorMsg);
        }
        return;
    }

    if (Ecp_CookieOtherDomain.length > 0) {
        cookie("Ecp_IpLoginFail", "", { expires: -1, path: '/', domain: Ecp_CookieOtherDomain }, false);
    }

    if (vj.Domain !== null && vj.Domain !== undefined && vj.Domain !== '') {
        //Ecp_CookieDomain = vj.Domain;
        //Ecp_CookieOtherDomain = vj.Domain;
    }

    $(".modal").hide();

    Ecp_IsLogin = true;

    var data = new Object();//for cookie
    if (b_AutoLogin)
        vj.IsAutoLogin = true;

    //if (!b_newLogin) {
    if (Ecp_ResultR === undefined) {
        var v = cookie(Ecp_LoginStuts);
        if (v && v.length > 0) {
            var vjson = JSON.parse(v);
            vj.r = vjson.r;
        }
    }
    else {
        vj.r = Ecp_ResultR;
    }
    //}

    vj.ShowName = decodeURIComponent(vj.ShowName);

    data.IsAutoLogin = vj.IsAutoLogin;
    data.UserName = vj.UserName;
    data.ShowName = encodeURIComponent(vj.ShowName);
    data.UserType = vj.UserType;
    data.r = vj.r;

    var zone = getClientTimezone();
    if (zone === 8) {

        var fromCookie = false;
        var expire;
        if (vj.Expire !== undefined && vj.Expire.length > 0)
            expire = vj.Expire;
        else {
            fromCookie = true;
            expire = cookie('c_m_expire');
        }

        var d;
        if (expire && expire.length > 0) {
            if (Ecp_CookieOtherDomain.length > 0 && fromCookie) {//从cookie获取的过期时间,当前时间加20
                d = new Date();
                d.setMinutes(d.getMinutes() + 20);
            }
            else {
                d = new Date(Date.parse(expire.replace(/-/g, "/")));
            }
        }
        else if (data.IsAutoLogin) {
            d = 7;
        } else {
            d = new Date();
            d.setMinutes(d.getMinutes() + 20);
        }
    }
    else {
        if (data.IsAutoLogin) {
            d = 7;
        } else {
            d = new Date();
            d.setMinutes(d.getMinutes() + 20);
        }
    }

    var rootPath = Ecp_getRootPath();
    var hid = JSON.stringify(data);

    if (vj.Uid === undefined) {
        var uid = cookie('c_m_LinID');
        if (uid !== null) {
            vj.Uid = getSubCookie(decodeURIComponent(uid), "LinID");
        }
    }

    var expireDate = new Date(d);
    expireDate.setSeconds(expireDate.getSeconds() + 5);

    //cookie.raw = true;

    cookie(Ecp_notFirstLogin, data.r, { expires: d, path: rootPath }, false);
    cookie(Ecp_LoginStuts, hid, { expires: d, path: '/', domain: Ecp_CookieDomain }, false);
    if (vj.Uid !== null)
        cookie('c_m_LinID', 'LinID=' + vj.Uid + '&ot=' + d.Format('MM/dd/yyyy HH:mm:ss'), { expires: d, path: '/', domain: Ecp_CookieDomain }, false);
    cookie('c_m_expire', d.Format('yyyy-MM-dd HH:mm:ss'), { expires: expireDate, path: '/', domain: Ecp_CookieDomain }, false);
    cookie("Ecp_lout", 0, { expires: -1, path: '/', domain: Ecp_CookieDomain }, false);

    if (Ecp_CookieOtherDomain.length > 0) {
        cookie(Ecp_LoginStuts, hid, { expires: d, path: '/', domain: Ecp_CookieOtherDomain }, false);
        if (vj.Uid !== null)
            cookie('c_m_LinID', 'LinID=' + vj.Uid + '&ot=' + d.Format('MM/dd/yyyy HH:mm:ss'), { expires: d, path: '/', domain: Ecp_CookieOtherDomain }, false);
        cookie('c_m_expire', d.Format('yyyy-MM-dd HH:mm:ss'), { expires: expireDate, path: '/', domain: Ecp_CookieOtherDomain }, false);
        cookie("Ecp_lout", 0, { expires: -1, path: '/', domain: Ecp_CookieOtherDomain }, false);
        cookie("Ecp_session", 1, { path: '/', domain: Ecp_CookieOtherDomain }, false);
    }

    //cookie("Ecp_test", "aa" + data.r, { expires: d, path: '/', domain: Ecp_CookieDomain },false)
    //cookie("Ecp_test2", "aa2" + data.r, { expires: d, path: '/' },false)

    //美国站点 需要后面的附加 , 地址为 http://cnkiecp.global.cnki.net.virtual.anu.edu.au/MyCNKI  在views里
    //青岛不需要附加 青岛my.cnki.net
    if (vj.Uid !== undefined) {
        $("#Ecp_MycnkiLink").attr("href", $("#Ecp_MycnkiLink").attr("href") + "/loginid2.aspx?uid=" + vj.Uid);
    }

    $("#Ecp_top_login_layer").hide();
    if (Ecp_PageStyle === 'header') {
        $("#Ecp_top_login").hide();
        $("#Ecp_top_logout_layer").hide();
        $("#Ecp_top_logout").show();
        $("#Ecp_TextBoxUserName").val("");
        $("#Ecp_TextBoxPwd").val("");
        $("#Ecp_CheckCode").val("");
        $("#rememberMe").attr("checked", false);

        $("#Ecp_loginShowName").text(vj.ShowName);

        $("#Ecp_header_Register").hide();
    }

    if (typeof getLeadHtml === 'function')
        getLeadHtml(Ecp_topLoginUrl, data.UserName, data.UserType, "", b_newLogin);

    if (typeof LoginSucess === 'function') {
        if (Ecp_PageStyle === 'header') {
            if (b_newLogin)
                LoginSucess(vj, b_newLogin);
        } else
            LoginSucess(vj, b_newLogin);
    }
}

function Ecp_LogoutOptr_my() {
    $("#Ecp_top_login").show();
    $("#Ecp_top_logout").hide();
    $("#Ecp_header_Register").show();

    Ecp_ShowMsg();


    Ecp_UserLogout();

    if (typeof Ecp_LogoutOptr === 'function')
        Ecp_LogoutOptr();
}

function Ecp_ReGetImg() {
    $("#Ecp_CheckCodeImg").attr("src", Ecp_topLoginUrl + "api/loginapi/CheckCode?t=" + Math.random());
}

//login validate--------------

String.prototype.Trim = function () {
    return this.replace(/^\s+|\s+$/g, "");
}
String.prototype.Ltrim = function () {
    return this.replace(/^\s+/g, "");
}
String.prototype.Rtrim = function () {
    return this.replace(/\s+$/g, "");
}
function RegexCheck(reg, str) {
    return reg.test(str);
}

function Ecp_CheckUserName(userName) {
    var uName = userName.val();
    uName = $.trim(uName);
    if (uName === '') {
        Ecp_ShowMsgFocus(getLoginResource("EmptyUsername"), userName);//'用户名不能为空，请输入。'
        return false;
    }
    if (RegexCheck(/.*('|;|\"|--).*/, uName) == true) {
        Ecp_ShowMsgFocus(getLoginResource("WrongFormatUsername"), userName);//'用户名格式不正确。'
        return false;
    }
    return true;
}
function Ecp_CheckPwd(pwd) {
    if (pwd.val() === '') {
        Ecp_ShowMsgFocus(getLoginResource("EmptyPassword"), pwd);//'密码不能为空，请输入。'
        return false;
    }
    return true;
}

function Ecp_EnterSubmit(e, invalue, button, pageStyle) {
    if (invalue.value && invalue.value === "") {
        Ecp_ShowMsg();
        return;
    }
    if (window.event)
        keyPressed = window.event.keyCode; // IE
    else
        keyPressed = e.which; // Firefox
    if (keyPressed === 13) {
        Ecp_SubmitCheck(null, button, pageStyle);
        return false;
    }
}


function Ecp_SubmitCheck(type, button, pageStyle) {
    Ecp_ShowMsg();
    var userName = $('#Ecp_TextBoxUserName');
    var pwd = $('#Ecp_TextBoxPwd');

    if (!Ecp_CheckUserName(userName))
        return false;
    if (!Ecp_CheckPwd(pwd))
        return false;

    //button.disabled = true;
    Ecp_UserLogin(userName.val(), pwd.val());
}

function Ecp_GetQueryString(name) {
    var reg = new RegExp("(^|&)" + name + "=([^&]*)(&|$)", "i");
    var r = window.location.search.substr(1).match(reg);
    if (r !== null) return unescape(r[2]); return null;
}

function Ecp_LoadJs(src, id, fun, parm) {
    var script = document.getElementById(id);
    if (script) {
        var head = document.getElementsByTagName('head')[0];
        head.removeChild(script);
    }

    script = document.createElement("script");
    script.id = id;
    script.type = "text/javascript";
    script.src = src;
    if (isImplementedOnload(script)) {
        script.onload = function () {
            fun(parm);
        };
    } else {
        script.onreadystatechange = function () {
            var r = script.readyState;
            if (r === 'loaded' || r === 'complete') {
                script.onreadystatechange = null;
                fun(parm);
            }
        };
    }
    document.getElementsByTagName("head")[0].appendChild(script);

}

function isImplementedOnload(script) {
    script = script || document.createElement('script');
    if ('onload' in script)
        return true;
    script.setAttribute('onload', '');
    return typeof script.onload === 'function';
}

function parseParam(param, key) {
    var paramStr = "";
    if (param instanceof String || param instanceof Number || param instanceof Boolean) {
        paramStr += "&" + key + "=" + encodeURIComponent(param);
    } else {
        $.each(param, function (i) {
            var k = key === null ? i : key + (param instanceof Array ? "[" + i + "]" : "." + i);
            paramStr += '&' + parseParam(this, k);
        });
    }
    return paramStr.substr(1);
}

function Ecp_ValdateInput(inp) {
    var el = document.getElementById(inp);
    if ("\v" == "v") {
        el.onpropertychange = textChange;
    } else {
        el.addEventListener("input", textChange, false);
    }
    function textChange() {
        if (el.value === "") {
            Ecp_ShowMsg();
        }
    }
}

function Ecp_getRootPath() {
    var strFullPath = window.document.location.href;
    var strPath = window.document.location.pathname;
    var pos = strFullPath.indexOf(strPath);
    var postPath = strPath.substring(0, strPath.substr(1).indexOf('/') + 1) + '/';
    return postPath;
}

function Ecp_TestCookieEnable() {
    var result = false;
    if (!navigator)
        return false;
    if (navigator.cookiesEnabled)
        return true;
    document.cookie = "e_t_c=1; expires=60";
    var cookieSet = document.cookie;
    if (cookieSet.indexOf("e_t_c=1") > -1)
        result = true;
    document.cookie = "";
    return result;
}
//----------------------
function cookie(key, value, options, isEncode) {
    if (typeof value === "undefined") {
        var cookies = document.cookie.split("; ");
        for (var i = 0, len = cookies.length; i < len; i++) {
            var parts = cookies[i].split("=");
            var name = decodeURIComponent(parts.shift());
            if (name === key)
                return decodeURIComponent(parts.join("="));
        }
        return undefined;
    }

    var _cookie = '';
    if (!isEncode)
        _cookie = key + "=" + value;
    else
        _cookie = encodeURIComponent(key) + "=" + encodeURIComponent(value);

    options = options || {};
    if (options.expires) {
        if (typeof options.expires === 'number') {
            var days = options.expires, t = options.expires = new Date();
            t.setTime(+t + days * 864e+5);
        }
        _cookie += ";expires=" + options.expires.toUTCString();
    }
    if (options.path)
        _cookie += ";path=" + options.path;
    if (options.domain)
        _cookie += ";domain=" + options.domain;
    //if (options.secure)
        _cookie += ";secure";
    document.cookie = _cookie;
}

function getSubCookie(coo, key) {
    if (coo !== undefined && key !== undefined) {
        var subCoo = coo.split("&");
        for (var i = 0; i < subCoo.length; i++) {
            var index = subCoo[i].indexOf("=");
            var sub = subCoo[i].substring(0, index);
            if (decodeURIComponent(sub) === key)
                return decodeURIComponent(subCoo[i].substring(index + 1));
        }
    }
    return "";
}

function getClientTimezone() {
    var oDate = new Date();
    var nTimezone = -oDate.getTimezoneOffset() / 60;
    return nTimezone.toFixed(2);
}
//----------------------