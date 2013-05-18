function parseTerm(searchTerm) {
  var parsedTerm = searchTerm.replace(/ near /g,'');
  parsedTerm = parsedTerm.replace(/ in /g,'');
  parsedTerm = parsedTerm.replace(/_/g,'');
  parsedTerm = parsedTerm.replace(/[^\w]/g,'').toUpperCase()
  return parsedTerm;
}

function findRank(element) {
  if("CITY" == element) return 100;
  if("SUBURB" == element) return 90;
  if("TOWN" == element) return 80;
  if("VILLAGE" == element) return 70;
  if("HAMLET" == element) return 60;
  if("LOCALITY" == element) return 50;
  if("MOOR" == element) return 40;
  if("FARM" == element) return 30;
  return 10;
}

print("Building Node Place Initial Terms");
var nodePlace = db.node.find({"tags.place": {$exists: true}, "tags.name": {$exists: true}});
nodePlace.forEach(function(entry) {
  if (entry.tags.place != "") {
    var parsedSearchTerm = parseTerm(entry.tags.name);
    var parsedType = parseTerm(entry.tags.place);
    var record = {   latitude: entry.latitude
                   , longitude: entry.longitude
                   , location: {longitude: entry.longitude, latitude: entry.latitude }
                   , searchTerm: parsedSearchTerm
                   , type: parsedType
                   , rank: findRank(parsedType)
                   , source: "OPENSTREETMAP-NODE-PLACES"
                 }

    db.OSMData_Places.save(record);
  }
}); 
db.OSMData_Places.ensureIndex({ location : "2d" });

print("Building Node Place Terms With Near Places");
var osmPlaces = db.OSMData_Places.find();
osmPlaces.forEach(function(row) {
  var near = db.OSMData_Places.find({location : { $nearSphere : row.location, $maxDistance : 0.00378906922 }}).limit(10000);
  var nearList = [];
  nearList.push(row.searchTerm);
  near.forEach(function(e) {
    nearList.push(row.searchTerm + e.searchTerm);
  });
  
  row.near = nearList;
  db.OSMData_Places_Near.save(row);
});

//--------Amenity
print("Building Node Amenity Initial Terms");
var nodeAmenity = db.node.find({"tags.amenity": {$exists: true}, "tags.name": {$exists: true}});
nodeAmenity.forEach(function(entry) {
  var parsedSearchTerm = parseTerm(entry.tags.name);
  var parsedType = parseTerm(entry.tags.amenity);
  if (parsedSearchTerm != "" && parsedType != "") {
    var record = {   latitude: entry.latitude
                       , longitude: entry.longitude
                   , location: {longitude: entry.longitude, latitude: entry.latitude }
                   , searchTerm: parsedSearchTerm
                   , type: parsedType
                   , rank: findRank(parsedType)
                   , source: "OPENSTREETMAP-NODE-AMENITIES"
                 }

    db.OSMData_Amenities.save(record);
  }
}); 
db.OSMData_Amenities.ensureIndex({ location : "2d" });

print("Building Node Amenities Terms With Near Places");
var osmAmenity = db.OSMData_Amenities.find();
osmAmenity.forEach(function(row) {
  var near = db.OSMData_Places.find({location : { $nearSphere : row.location, $maxDistance : 0.00126302307335 }}).limit(10000);
  var nearList = [];
  nearList.push(row.searchTerm);
  near.forEach(function(e) {
    nearList.push(row.searchTerm + e.searchTerm);
  });
  
  row.near = nearList;
  db.OSMData_Amenities_Near.save(row);
});


// print("Dropping Existing Search Terms");
// db.Location.drop();
// 
// print("Building PostalCode Terms");
// print("We store location as longitude and latitude for the geo spatial index");
// var postalCodes = db.PostalCode.find();
// postalCodes.forEach(function(r) {
//   var original = r.postalCode;
//   var term = parseTerm(original);
//   var newRecord = { searchTerm: [term]
//                   , latitude: r.latitude
//                   , longitude: r.longitude
//                   , location: { lon : r.longitude, lat : r.latitude }
//                   , rank: 10
//                   , source: "GEONAMES-POSTCODE"
//                   };
//   db.Location.save(newRecord);
// });

print("Building Streetmap Place Terms Based on Places and Amenities");
print("We store location as longitude and latitude for the geo spatial index");
function buildOSMEntry(data) {
  var newRecord = { searchTerm: data.near
                  , latitude: data.latitude
                  , longitude: data.longitude
                  , location: { lon : data.longitude, lat : data.latitude }
                  , rank: data.rank
                  , source: data.source
                  };
 db.Location.save(newRecord); 
}
var places = db.OSMData_Places_Near.find();
var amenities = db.OSMData_Amenities_Near.find();

places.forEach(buildOSMEntry);
amenities.forEach(buildOSMEntry);

// print("Building Collected Places Terms");
// print("We store location as longitude and latitude for the geo spatial index");
// var records = db.CollectedPlaces.find();
// records.forEach(function(r) {
//   var original = r.name;
//   var term = parseTerm(original);
//   var newRecord = { searchTerm: [term]
//                   , latitude: r.latitude
//                   , longitude: r.longitude
//                   , location: { lon : r.longitude, lat : r.latitude }
//                   , rank: 10
//                   , source: "COLLECTED-PLACES"
//                   }
//   db.Location.save(newRecord);
// });

db.Location.ensureIndex({ searchTerm:1 });
db.Location.ensureIndex({ location: "2d" });
