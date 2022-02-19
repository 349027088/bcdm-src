package com.bcdm.foodtraceability.controller;


import com.alibaba.fastjson.JSONObject;
import com.bcdm.foodtraceability.configuration.BlogAction;
import com.bcdm.foodtraceability.entity.*;
import com.bcdm.foodtraceability.service.CityService;
import org.springframework.web.bind.annotation.*;

import java.util.List;

import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_SUCCESS;
import static com.bcdm.foodtraceability.common.MessageConstants.GET_CITY_LIST_SUCCESS;

/**
 * <p>
 * 城市信息前端控制器X
 * </p>
 *
 * @author 王
 * @since 2022-02-12
 */
@RestController
@RequestMapping("/city")
public class CityController {

    private final CityService cityService;

    public CityController(CityService cityService) {
        this.cityService = cityService;
    }

    /**
     * 生产产地信息查询Controller
     *
     * @return 所有城市信息
     * @throws Exception 查询信息失败
     */
    @PostMapping("/get")
    @CrossOrigin
    public ReturnItem<List<CityModel>> getCityList(@RequestBody String selectInfo) throws Exception {
        BlogAction.logger.info("企业:"+ JSONObject.parseObject(selectInfo).getInteger("companyId") +"----获取城市信息列表");
        ReturnItem<List<CityModel>> returnItem = new ReturnItem<>();
        returnItem.setT(cityService.getCityList());
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(GET_CITY_LIST_SUCCESS);
        return returnItem;
    }
}

