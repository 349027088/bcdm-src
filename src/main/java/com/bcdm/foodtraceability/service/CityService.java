package com.bcdm.foodtraceability.service;

import com.bcdm.foodtraceability.entity.City;
import com.baomidou.mybatisplus.extension.service.IService;
import com.bcdm.foodtraceability.entity.CityModel;

import java.util.List;

/**
 * <p>
 *  城市信息服务类
 * </p>
 *
 * @author 王
 * @since 2022-02-12
 */
public interface CityService extends IService<City> {

    /**
     * 获取城市信息列表
     *
     * @return 成功获取的城市信息列表
     */
    List<CityModel> getCityList() throws Exception;
}
