package com.bcdm.foodtraceability.service.impl;

import com.bcdm.foodtraceability.entity.City;
import com.bcdm.foodtraceability.entity.CityModel;
import com.bcdm.foodtraceability.exception.ServiceBusinessException;
import com.bcdm.foodtraceability.mapper.CityMapper;
import com.bcdm.foodtraceability.service.CityService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

import java.util.*;

import static com.bcdm.foodtraceability.common.Constants.SELECT_ZERO;
import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_FAIL;
import static com.bcdm.foodtraceability.common.MessageConstants.GET_CITY_LIST_FAIL;

/**
 * <p>
 * 城市信息服务实现类
 * </p>
 *
 * @author 王
 * @since 2022-02-12
 */
@Service
public class CityServiceImpl extends ServiceImpl<CityMapper, City> implements CityService {

    @Override
    public List<CityModel> getCityList() throws Exception {
        List<City> cityList = list();
        if (null != cityList && SELECT_ZERO != cityList.size()) {
            Integer typeInt = 0;
            List<CityModel> proList = new ArrayList<>();
            CityModel proCity = new CityModel();
            proCity.setChildren(new ArrayList<>());
            for (int i = 0; i < cityList.size(); i++) {
                if (!Objects.equals(typeInt, cityList.get(i).getProId())) {
                    typeInt = cityList.get(i).getProId();
                    proCity = new CityModel();
                    proCity.setId(cityList.get(i).getProId());
                    proCity.setName(cityList.get(i).getProName());
                    proCity.setChildren(new ArrayList<>());
                    proList.add(proCity);
                }
                CityModel city = new CityModel();
                city.setId(cityList.get(i).getCityId());
                city.setName(cityList.get(i).getCityName());
                proCity.getChildren().add(city);
            }
            return proList;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, GET_CITY_LIST_FAIL);
    }
}
