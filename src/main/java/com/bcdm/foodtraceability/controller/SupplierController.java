package com.bcdm.foodtraceability.controller;


import com.bcdm.foodtraceability.entity.Company;
import com.bcdm.foodtraceability.entity.ReturnItem;
import com.bcdm.foodtraceability.entity.Supplier;
import com.bcdm.foodtraceability.service.SupplierService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;


import java.util.List;

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
@Slf4j
public class SupplierController {

    private final SupplierService supplierService;

    public SupplierController(SupplierService supplierService) {
        this.supplierService = supplierService;
    }

    /**
     * 增加供应商信息Controller
     *
     * @param supplier 需要添加的供应想信息
     * @return 创建成功的供应商信息
     * @throws Exception 增加供应商失败
     */
    @PostMapping("/create")
    @CrossOrigin
    public ReturnItem<Boolean> create(@RequestBody Supplier supplier) throws Exception {
        log.info(supplier.toString());
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
     * @return 修改成功的供应商信息
     * @throws Exception 修改失败
     */
    @PostMapping("/modify")
    @CrossOrigin
    public ReturnItem<Boolean> modify(@RequestBody Supplier supplier) throws Exception {
        log.info(supplier.toString());
        ReturnItem<Boolean> returnItem = new ReturnItem<>();
        returnItem.setT(supplierService.modifySupplier(supplier));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(MODIFY_SUPPLIER_INFO_SUCCESS);
        return returnItem;
    }

    /**
     * 删除供应商信息Controller
     *
     * @param supplier 用户账号密码和新密码
     * @return 修改成功的用户信息
     * @throws Exception 修改失败
     */
    @PostMapping("/delete")
    @CrossOrigin
    public ReturnItem<Boolean> delete(@RequestBody Supplier supplier) throws Exception {
        log.info(supplier.toString());
        ReturnItem<Boolean> returnItem = new ReturnItem<>();
        returnItem.setT(supplierService.deleteSupplier(supplier));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(DELETE_SUPPLIER_INFO_SUCCESS);
        return returnItem;
    }

    /**
     * 获取公司的所有未删除供应商列表信息
     *
     * @param company 需要获取供应商列表的企业
     * @return 获取供应商列表
     * @throws Exception 获取信息失败
     */
    @PostMapping("/getSupplierList")
    @CrossOrigin
    public ReturnItem<List<Supplier>> getSupplierList(@RequestBody Company company) throws Exception {
        log.info(company.toString());
        ReturnItem<List<Supplier>> returnItem = new ReturnItem<>();
        returnItem.setT(supplierService.getSupplierList(company));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(SELECT_SUPPLIER_INFO_SUCCESS);
        return returnItem;
    }

}

