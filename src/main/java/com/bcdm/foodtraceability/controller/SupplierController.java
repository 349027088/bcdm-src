package com.bcdm.foodtraceability.controller;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.bcdm.foodtraceability.configuration.BlogAction;
import com.bcdm.foodtraceability.entity.Supplier;
import com.bcdm.foodtraceability.entity.ReturnItem;
import com.bcdm.foodtraceability.entity.SelectPageEntity;
import com.bcdm.foodtraceability.service.SupplierService;
import com.bcdm.foodtraceability.validatedgroup.*;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_SUCCESS;
import static com.bcdm.foodtraceability.common.MessageConstants.*;

/**
 * <p>
 * 供应商前端控制器
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@RestController
@RequestMapping("/supplier")
public class SupplierController {

    private final SupplierService supplierService;

    public SupplierController(SupplierService supplierService) {
        this.supplierService = supplierService;
    }

    /**
     * 获取某条件的公司未删除供应商列表信息
     *
     * @param selectInfo 查询条件
     * @return 获取供应商列表
     * @throws Exception 获取信息失败
     */
    @PostMapping("/getSupplierList")
    @CrossOrigin
    public ReturnItem<IPage<Supplier>> getSupplierList(@RequestBody String selectInfo) throws Exception {
        SelectPageEntity<Supplier> selectPageEntity = new SelectPageEntity<>(selectInfo);
        BlogAction.logger.info("企业:" + selectPageEntity.getCompanyId() + "-----获取所有供应商信息");
        ReturnItem<IPage<Supplier>> returnItem = new ReturnItem<>();
        returnItem.setT(supplierService.getSupplierList(selectPageEntity));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(SELECT_SUPPLIER_INFO_SUCCESS);
        return returnItem;
    }

    /**
     * 获取指定的供应商列表信息
     *
     * @param supplier 需要获取供应商信息的企业
     * @return 查询到的供应商信息
     * @throws Exception 查询失败
     */
    @PostMapping("/getSupplierById")
    @CrossOrigin
    public ReturnItem<Supplier> getSupplierById(@Validated({GetInfoGroup.class})
                                                @RequestBody Supplier supplier) throws Exception {
        BlogAction.logger.info("企业:" + supplier.getCompanyId() + "-----获取编号:" + supplier.getSupplierId() + "的供应商信息");
        ReturnItem<Supplier> returnItem = new ReturnItem<>();
        returnItem.setT(supplierService.getSupplierById(supplier));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(SELECT_SUPPLIER_INFO_SUCCESS);
        return returnItem;
    }

    /**
     * 增加供应商信息Controller
     *
     * @param supplier 需要添加的供应想信息
     * @return true 创建成功
     * @throws Exception 创建失败
     */
    @PostMapping("/create")
    @CrossOrigin
    public ReturnItem<Boolean> create(@Validated({CreateGroup.class})
                                      @RequestBody Supplier supplier) throws Exception {
        BlogAction.logger.info("企业:" + supplier.getCompanyId() + "-----添加供应商:" + supplier.getSupplierName());
        ReturnItem<Boolean> returnItem = new ReturnItem<>();
        returnItem.setT(supplierService.createSupplier(supplier));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(ADD_SUPPLIER_INFO_SUCCESS);
        return returnItem;
    }

    /**
     * 修改供应商信息Controller
     *
     * @param supplier 需要修改的供应商信息
     * @return true 修改成功
     * @throws Exception 修改失败
     */
    @PostMapping("/modify")
    @CrossOrigin
    public ReturnItem<Boolean> modify(@Validated({ModifyGroup.class})
                                      @RequestBody Supplier supplier) throws Exception {
        BlogAction.logger.info("企业:" + supplier.getCompanyId() + "-----修改供应商:" + supplier.getSupplierId() + "-----名称:" + supplier.getSupplierName());
        ReturnItem<Boolean> returnItem = new ReturnItem<>();
        returnItem.setT(supplierService.modifySupplier(supplier));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(MODIFY_SUPPLIER_INFO_SUCCESS);
        return returnItem;
    }

    /**
     * 删除供应商信息Controller
     *
     * @param supplier 需要删除的供应商
     * @return true 删除成功
     * @throws Exception 删除失败
     */
    @PostMapping("/delete")
    @CrossOrigin
    public ReturnItem<Boolean> delete(@Validated({DeleteGroup.class})
                                      @RequestBody Supplier supplier) throws Exception {
        BlogAction.logger.info("企业:" + supplier.getCompanyId() + "-----删除供应商:" + supplier.getSupplierName());
        ReturnItem<Boolean> returnItem = new ReturnItem<>();
        returnItem.setT(supplierService.deleteSupplier(supplier));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(DELETE_SUPPLIER_INFO_SUCCESS);
        return returnItem;
    }

}

