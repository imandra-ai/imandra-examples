function ConstructGroups(regions) {
    var annotated = [];
    regions = regions.filter(function(r){ return ("name" in r); })
    regions.forEach(function(region){
         annotated.push({ path: region.name.split("."), region:region });
    });

    function buildMaps(ann) {
        var map = d3.map();
        ann.forEach(function(r){
            var n = r.path.shift();
            if(n == undefined) return;
            if(!map.has(n)) map.set(n, []);
            map.get(n).push(r);
        });
        map.keys().forEach(function(k){
            var v = map.get(k);
            if(v.length == 1) map.set(k, v[0].region);
            else map.set(k, buildMaps(v));
        });
        return map;
    };

    var hmap = buildMaps(annotated);

    function convert(map, clabel) {
        var groups = [];
        if("name" in map) { return { label: name }; }
        else 
        {
            map.each(function(v,k){
                var newLabel = k;
                if(clabel != "") newLabel = clabel + "." + k;
                var entry = {label: newLabel};
                if( "name" in v ){ 
                    entry["label"] = "Region " + entry["label"];
                    entry["payload"] = v;
                    entry["weight"] = 1;
                } else {
                    entry["groups"] = convert(v, newLabel);
                    entry["weight"] = entry["groups"].length;
                }
                groups.push(entry);
            });
        }
        return groups; 
    };
    return convert(hmap,"");
};
 
