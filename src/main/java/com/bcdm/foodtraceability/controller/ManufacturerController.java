package com.bcdm.foodtraceability.controller;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.bcdm.foodtraceability.configuration.BlogAction;
import com.bcdm.foodtraceability.entity.Manufacturer;
import com.bcdm.foodtraceability.entity.ReturnItem;
import com.bcdm.foodtraceability.entity.SelectPageEntity;
import com.bcdm.foodtraceability.service.ManufacturerService;
import com.bcdm.foodtraceability.validatedgroup.*;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

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
     * 获取某条件的公司生产厂商信息
     *
     * @param selectInfo 查询条件
     * @return 获取生产厂商列表
     * @throws Exception 查询信息失败或者结果为0条信息
     */
    @PostMapping("/getManufacturerList")
    @CrossOrigin
    public ReturnItem<IPage<Manufacturer>> getManufacturerList(@RequestBody String selectInfo) throws Exception {
        SelectPageEntity<Manufacturer> selectPageEntity = new SelectPageEntity<>(selectInfo);
        BlogAction.logger.info("企业" + selectPageEntity.getCompanyId() + "-----获取所有生产厂商信息");
        ReturnItem<IPage<Manufacturer>> returnItem = new ReturnItem<>();
        returnItem.setT(manufacturerService.getManufacturerList(selectPageEntity));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(SELECT_MANUFACTURER_SUCCESS);
        return returnItem;
    }

    /**
     * 获取公司的指定生产厂商信息
     *
     * @param getOneInfo 获取公司的指定商品种类的Id和公司Id
     * @return 指定ID的生产厂商信息
     * @throws Exception 查询失败
     */
    @PostMapping("/getManufacturerById")
    @CrossOrigin
    public ReturnItem<Manufacturer> getManufacturerById(@Validated({GetInfoGroup.class})
                                                        @RequestBody Manufacturer getOneInfo) throws Exception {
        BlogAction.logger.info("企业" + getOneInfo.getCompanyId() + "-----获取编号:" + getOneInfo.getManufacturerId() + "的生产厂商信息");
        ReturnItem<Manufacturer> returnItem = new ReturnItem<>();
        returnItem.setT(manufacturerService.getManufacturerById(getOneInfo));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(SELECT_MANUFACTURER_SUCCESS);
        return returnItem;
    }

    /**
     * 增加生产厂商信息Controller
     *
     * @param manufacturer 需要添加的生产厂商信息
     * @return true 创建成功
     * @throws Exception 创建失败
     */
    @PostMapping("/create")
    @CrossOrigin
    public ReturnItem<Boolean> create(@Validated({CreateGroup.class})
                                      @RequestBody Manufacturer manufacturer) throws Exception {
        BlogAction.logger.info("企业" + manufacturer.getCompanyId() + "-----创建新生产厂商:" + manufacturer.getManufacturerName());
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
     * @return true 修改成功
     * @throws Exception 修改失败
     */
    @PostMapping("/modify")
    @CrossOrigin
    public ReturnItem<Boolean> modify(@Validated(ModifyGroup.class)
                                      @RequestBody Manufacturer manufacturer) throws Exception {
        BlogAction.logger.info("企业" + manufacturer.getCompanyId() + "-----修改生产厂商编号:" + manufacturer.getManufacturerId() + "-----名称:" + manufacturer.getManufacturerName());
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
     * @return true 删除成功
     * @throws Exception 删除失败
     */
    @PostMapping("/delete")
    @CrossOrigin
    public ReturnItem<Boolean> delete(@Validated(DeleteGroup.class)
                                      @RequestBody Manufacturer manufacturer) throws Exception {
        BlogAction.logger.info("企业" + manufacturer.getCompanyId() + "-----删除生产厂商:" + manufacturer.getManufacturerName());
        ReturnItem<Boolean> returnItem = new ReturnItem<>();
        returnItem.setT(manufacturerService.deleteManufacturer(manufacturer));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(DELETE_MANUFACTURER_SUCCESS);
        return returnItem;
    }

}

