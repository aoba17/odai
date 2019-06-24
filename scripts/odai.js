window.onload = function(){
    var dateObj = new Date();
    var year = dateObj.getFullYear();
    var month = dateObj.getMonth() + 1;
    var date = dateObj.getDate();
    var day = "日月火水木金土".charAt(dateObj.getDay());
    document.getElementById("currentDate")
        .innerHTML = year+"年"+month+"月"+date+"日("+day+")";
}
