package com.bcdm.foodtraceability.controller;


import com.bcdm.foodtraceability.entity.Company;
import com.bcdm.foodtraceability.entity.GoodsType;
import com.bcdm.foodtraceability.entity.Manufacturer;
import com.bcdm.foodtraceability.entity.ReturnItem;
import com.bcdm.foodtraceability.service.GoodsTypeService;
import com.bcdm.foodtraceability.service.ManufacturerService;
import org.springframework.web.bind.annotation.*;

import org.springframework.stereotype.Controller;

import java.util.List;

import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_SUCCESS;
import static com.bcdm.foodtraceability.common.MessageConstants.*;

/**
 * <p>
 * 前端控制器
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@RestController
@RequestMapping("/manufacturer")
public class ManufacturerController {


    private final ManufacturerService manufacturerService;

    public ManufacturerController(ManufacturerService manufacturerService) {
        this.manufacturerService = manufacturerService;
    }

    /**
     * 增加商品种类信息Controller
     *
     * @param manufacturer 需要添加的供应想信息
     * @return 创建成功的供应商信息
     */
    @PostMapping("/create")
    @CrossOrigin
    public ReturnItem<Boolean> create(@RequestBody Manufacturer manufacturer) {
        ReturnItem<Boolean> returnItem = new ReturnItem<>();
        returnItem.setT(manufacturerService.createManufacturer(manufacturer));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(ADD_SUPPLIER_INFO_SUCCESS);
        return returnItem;
    }

    /**
     * 删除商品种类信息Controller
     *
     * @param manufacturer 需要删除的商品种类
     * @return 删除状态
     */
    @PostMapping("/delete")
    @CrossOrigin
    public ReturnItem<Boolean> delete(@RequestBody Manufacturer manufacturer) {
        ReturnItem<Boolean> returnItem = new ReturnItem<>();
        returnItem.setT(manufacturerService.deleteManufacturer(manufacturer));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(DELETE_SUPPLIER_INFO_SUCCESS);
        return returnItem;
    }

    /**
     * 获取公司的所有商品种类信息
     *
     * @param company 需要获取供应商列表的企业
     * @return 获取商品种类列表
     */
    @PostMapping("/getManufacturerList")
    @CrossOrigin
    public ReturnItem<List<Manufacturer>> getManufacturerList(@RequestBody Company company) {
        ReturnItem<List<Manufacturer>> returnItem = new ReturnItem<>();
        returnItem.setT(manufacturerService.getManufacturerList(company.getCompanyId()));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(SELECT_SUPPLIER_INFO_SUCCESS);
        return returnItem;
    }

}

