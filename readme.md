# 获取隧道列表 `GET /tunnels`

```
GET /tunnels HTTP/1.1
Accept: */*
Accept-Encoding: gzip, deflate
Connection: keep-alive
Host: 172.19.2.200:8081
User-Agent: HTTPie/0.9.3

HTTP/1.1 200 OK
Content-Type: application/json; charset=utf-8
Date: Fri, 27 May 2016 04:13:59 GMT
Server: tman/0.1
Transfer-Encoding: chunked
```

```json
[
    {
        "dns1": "202.102.224.68",      //对端推送的 DNS 服务器地址
        "dns2": "202.102.227.68",
        "external": "42.235.188.158",  //对端出口 IP
        "id": "pptp2",                 //隧道名称 （a-zA-Z0-9)
        "local": "172.17.0.2",         //本地网卡地址 （目前没啥用）
        "port": 32772,                 //代理端口 （自动分配）
        "server": "112.83.69.201",     //PPTP 服务器地址
        "status": "CONNECTED",         //状态 CONNECTED|DISCONNECTED|INITIAL
        "tunnel_ip": "172.16.254.7",   //隧道 IP 地址（没什么用）
        "user": "a051602"              //用户名
    },
    {
        "dns1": "202.103.24.68",
        "dns2": "202.103.44.150",
        "external": "27.24.32.108",
        "id": "pptp1",
        "local": "172.17.0.3",
        "port": -1,
        "server": "222.186.31.224",
        "status": "DISCONNECTED",
        "tunnel_ip": "12.12.12.11",
        "user": "a051601"
    }
]
```

# 获取隧道信息 `GET /tunnel/[:name]`

```
GET /tunnel/pptp1 HTTP/1.1
Accept: */*
Accept-Encoding: gzip, deflate
Connection: keep-alive
Host: 172.19.2.200:8081
User-Agent: HTTPie/0.9.3

HTTP/1.1 200 OK
Content-Type: application/json; charset=utf-8
Date: Fri, 27 May 2016 04:18:13 GMT
Server: tman/0.1
Transfer-Encoding: chunked
```

```json
{
    "dns1": "202.103.24.68",
    "dns2": "202.103.44.150",
    "external": "27.24.32.108",
    "id": "pptp1",
    "local": "172.17.0.3",
    "port": -1,
    "server": "222.186.31.224",
    "status": "DISCONNECTED",
    "tunnel_ip": "12.12.12.11",
    "user": "a051601"
}
```

# 创建隧道 `POST /tunnel`

## 参数

 - name 隧道名称
 - server 服务器地址
 - user 用户名
 - pass 密码
 
 
```
POST /tunnel HTTP/1.1
Accept: */*
Accept-Encoding: gzip, deflate
Connection: keep-alive
Content-Length: 54
Content-Type: application/x-www-form-urlencoded; charset=utf-8
Host: proxy-zw29-01:8081
User-Agent: HTTPie/0.9.3

name=pptp2&server=112.83.69.201&user=a051602&pass=****

HTTP/1.1 200 OK
Content-Type: application/json; charset=utf-8
Date: Fri, 27 May 2016 04:23:59 GMT
Server: tman/0.1
Transfer-Encoding: chunked
```

```json
{
    "dns1": "202.102.224.68",
    "dns2": "202.102.227.68",
    "external": "42.235.188.158",
    "id": "pptp2",
    "local": "172.17.0.2",
    "port": 32773,
    "server": "112.83.69.201",
    "status": "CONNECTED",
    "tunnel_ip": "172.16.254.7",
    "user": "a051602"
}
```

# 删除隧道 `DELETE /tunnel/[:name]`

```
DELETE /tunnel/pptp2 HTTP/1.1
Accept: */*
Accept-Encoding: gzip, deflate
Connection: keep-alive
Content-Length: 0
Host: proxy-zw29-01:8081
User-Agent: HTTPie/0.9.3

HTTP/1.1 200 OK
Content-Type: text/plain; charset=utf-8
Date: Fri, 27 May 2016 04:25:13 GMT
Server: tman/0.1
Transfer-Encoding: chunked

pptp2
```

# 重播（更换出口 IP） `POST /tunnel/[:name]/redial`

```
POST /tunnel/pptp1/redial HTTP/1.1
Accept: */*
Accept-Encoding: gzip, deflate
Connection: keep-alive
Content-Length: 0
Host: proxy-zw29-01:8081
User-Agent: HTTPie/0.9.3

HTTP/1.1 200 OK
Content-Type: application/json; charset=utf-8
Date: Fri, 27 May 2016 04:27:18 GMT
Server: tman/0.1
Transfer-Encoding: chunked
```

```json
{
    "dns1": "202.101.224.69",
    "dns2": "202.101.226.69",
    "external": "",
    "id": "pptp1",
    "local": "172.17.0.2",
    "port": 32774,
    "server": "222.186.31.224",
    "status": "CONNECTED",
    "tunnel_ip": "12.12.12.33",
    "user": "a051601"
}
```

# 停用隧道 `POST /tunnel/[:name]/down`

```
POST /tunnel/pptp1/down HTTP/1.1
Accept: */*
Accept-Encoding: gzip, deflate
Connection: keep-alive
Content-Length: 0
Host: proxy-zw29-01:8081
User-Agent: HTTPie/0.9.3

HTTP/1.1 200 OK
Content-Type: application/json; charset=utf-8
Date: Fri, 27 May 2016 04:28:22 GMT
Server: tman/0.1
Transfer-Encoding: chunked
```

```json
{
    "dns1": "119.6.6.6",
    "dns2": "114.114.114.114",
    "external": "119.7.82.34",
    "id": "pptp1",
    "local": "172.17.0.2",
    "port": -1,
    "server": "222.186.31.224",
    "status": "DISCONNECTED",
    "tunnel_ip": "12.12.12.34",
    "user": "a051601"
}
```

# 启用隧道 `POST /tunnel/[:name]/up`

```
POST /tunnel/pptp1/up HTTP/1.1
Accept: */*
Accept-Encoding: gzip, deflate
Connection: keep-alive
Content-Length: 0
Host: proxy-zw29-01:8081
User-Agent: HTTPie/0.9.3

HTTP/1.1 200 OK
Content-Type: application/json; charset=utf-8
Date: Fri, 27 May 2016 04:29:21 GMT
Server: tman/0.1
Transfer-Encoding: chunked
```

```json
{
    "dns1": null,
    "dns2": null,
    "external": null,
    "id": "pptp1",
    "local": "172.17.0.2",
    "port": 32775,
    "server": "222.186.31.224",
    "status": "INITIAL",
    "tunnel_ip": null,
    "user": "a051601"
}
```
