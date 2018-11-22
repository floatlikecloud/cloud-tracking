GF = gfortran -g -fbacktrace -fcheck=all -Wall
NCDF = -L/opt/netcdf413/lib  -lnetcdf -lnetcdff
ICDF = -I/opt/netcdf413/include
MOD_cloud_labeling = useful_module.f95 input_netcdf_module.f95 cloudList_type.f95 sct1UFT_3d_module.f95 unionFindTree_module.f95 secondScan_3d_module.f95
MOD_get_cloud_property = useful_module.f95 input_netcdf_module.f95 cloudList_type.f95
MOD_timeHistory = useful_module.f95 input_netcdf_module.f95 cloudList_type.f95
MOD_isDeepConvSeed = useful_module.f95 input_netcdf_module.f95 cloudList_type.f95
MOD_searchFamilyUnion = useful_module.f95 input_netcdf_module.f95 cloudList_type.f95 family_search_module.f95
MOD_searchSingleFamily = useful_module.f95 input_netcdf_module.f95 cloudList_type.f95 family_search_module.f95

cloud_labeling.out: cloud_labeling.f95 ${MOD_cloud_labeling} 
	${GF} ${NCDF} ${ICDF} -O2 ${MOD_cloud_labeling} cloud_labeling.f95 -o cloud_labeling.out

timeHistory.out: timeHistory.f95 ${MOD_timeHistory}
	${GF} ${NCDF} ${ICDF} -O2 ${MOD_timeHistory} timeHistory.f95 -o timeHistory.out

isDeepConvSeed.out: isDeepConvSeed.f95 ${MOD_isDeepConvSeed}
	${GF} ${NCDF} ${ICDF} -O2 ${MOD_isDeepConvSeed} isDeepConvSeed.f95 -o isDeepConvSeed.out

searchFamilyUnion.out: searchFamilyUnion.f95 ${MOD_searchFamilyUnion}
	${GF} ${NCDF} ${ICDF} -O2 ${MOD_searchFamilyUnion} searchFamilyUnion.f95 -o searchFamilyUnion.out

searchSingleFamily.out: searchSingleFamily.f95 ${MOD_searchSingleFamily}
	${GF} ${NCDF} ${ICDF} -O2 ${MOD_searchSingleFamily} searchSingleFamily.f95 -o searchSingleFamily.out


get_cloud_property.out: get_cloud_property.f95 ${MOD_get_cloud_property} 
	${GF} ${NCDF} ${ICDF} -O2 ${MOD_get_cloud_property} get_cloud_property.f95 -o get_cloud_property.out
