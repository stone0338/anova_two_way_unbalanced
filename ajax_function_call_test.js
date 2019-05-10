var data_anova = {"factor1": ["M","M","M","M","M","M","M","M","M","F","F","F","F","F","F","F","F","F"],"factor2": ["l","l","l","m","m","m","h","h","h","l","l","l","m","m","m","h","h","h"],"value": [null,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18]}
var data_json = JSON.stringify(data_anova)
var xhttp2, key, url_new, anova_output, output_text
var xhttp = new XMLHttpRequest();

xhttp.open("POST", "http://localhost:5656/ocpu/library/GraphRobot/R/anova_two_way", true);
xhttp.onreadystatechange = function() {
    if (this.readyState == 4) {
        key = xhttp.getResponseHeader("x-ocpu-session")
        url_new = "http://localhost:5656/ocpu/tmp/" + key + "/stdout"
        xhttp2 = new XMLHttpRequest();
        xhttp2.open("GET", url_new, true);
        xhttp2.onreadystatechange = function() {
            if (this.readyState == 4) {
                output_text = xhttp2.responseText.trimRight()
                output_text = output_text.substring(5,output_text.length-1)
                anova_output = output_text.split("&")

            }
        }
        xhttp2.send()
    }
}
xhttp.setRequestHeader("Content-Type", "application/json");
xhttp.send(data_json)




