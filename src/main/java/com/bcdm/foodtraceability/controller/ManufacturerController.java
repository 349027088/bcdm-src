package com.bcdm.foodtraceability.controller;


import com.bcdm.foodtraceability.entity.Manufacturer;
import com.bcdm.foodtraceability.entity.ReturnItem;
import com.bcdm.foodtraceability.service.ManufacturerService;
import org.springframework.web.bind.annotation.*;

import java.util.List;

import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_SUCCESS;
import static com.bcdm.foodtraceability.common.MessageConstants.*;

/**
 * <p>
 * 生产厂商前端控制器
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
     * 增加生产厂商信息Controller
     *
     * @param manufacturer 需要添加的生产厂商信息
     * @return true 成功执行
     * @throws Exception 增加生产厂商信息失败
     */
    @PostMapping("/create")
    @CrossOrigin
    public ReturnItem<Boolean> create(@RequestBody Manufacturer manufacturer) throws Exception {
        ReturnItem<Boolean> returnItem = new ReturnItem<>();
        returnItem.setT(manufacturerService.createManufacturer(manufacturer));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(ADD_MANUFACTURER_SUCCESS);
        return returnItem;
    }

    /**
     * 修改生产厂商信息Controller
     *
     * @param manufacturer 需要修改的生产厂商
     * @return true 成功执行
     * @throws Exception 修改生产厂商信息失败
     */
    @PostMapping("/modify")
    @CrossOrigin
    public ReturnItem<Boolean> modify(@RequestBody Manufacturer manufacturer) throws Exception {
        ReturnItem<Boolean> returnItem = new ReturnItem<>();
        returnItem.setT(manufacturerService.modifyManufacturer(manufacturer));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(MODIFY_MANUFACTURER_SUCCESS);
        return returnItem;
    }

    /**
     * 删除生产厂商信息Controller
     *
     * @param manufacturer 需要删除的生产厂商
     * @return true 成功执行
     * @throws Exception 删除生产厂商失败
     */
    @PostMapping("/delete")
    @CrossOrigin
    public ReturnItem<Boolean> delete(@RequestBody Manufacturer manufacturer) throws Exception {
        ReturnItem<Boolean> returnItem = new ReturnItem<>();
        returnItem.setT(manufacturerService.deleteManufacturer(manufacturer));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(DELETE_MANUFACTURER_SUCCESS);
        return returnItem;
    }

    /**
     * 获取公司的所有生产厂商信息
     *
     * @param manufacturer 包含需要获取供应商列表的企业信息
     * @return 获取生产厂商列表
     * @throws Exception 获取生产厂商信息失败或者没有生产厂商的信息
     */
    @PostMapping("/getManufacturerList")
    @CrossOrigin
    public ReturnItem<List<Manufacturer>> getManufacturerList(@RequestBody Manufacturer manufacturer) throws Exception {
        ReturnItem<List<Manufacturer>> returnItem = new ReturnItem<>();
        returnItem.setT(manufacturerService.getManufacturerList(manufacturer.getCompanyId()));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(SELECT_MANUFACTURER_SUCCESS);
        return returnItem;
    }

}

