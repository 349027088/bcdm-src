package com.bcdm.foodtraceability.controller;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.bcdm.foodtraceability.configuration.BlogAction;
import com.bcdm.foodtraceability.entity.Empower;
import com.bcdm.foodtraceability.entity.ReturnItem;
import com.bcdm.foodtraceability.entity.SelectPageEntity;
import com.bcdm.foodtraceability.service.EmpowerService;
import org.springframework.web.bind.annotation.*;

import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_SUCCESS;
import static com.bcdm.foodtraceability.common.MessageConstants.SELECT_GOODS_TYPE_INFO_SUCCESS;

/**
 * <p>
 *  授权信息前端控制器
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@RestController
@RequestMapping("/empower")
public class EmpowerController {

    private final EmpowerService empowerService;

    public EmpowerController(EmpowerService empowerService) {
        this.empowerService = empowerService;
    }

    /**
     * 获取公司的所有商品种类信息
     *
     * @param selectInfo 装载用于查询所有商品种类的企业ID,查询条件
     * @return 获取商品种类列表
     * @throws Exception 查询信息失败或者结果为0条信息
     */
    @PostMapping("/getGoodsTypeList")
    @CrossOrigin
    public ReturnItem<IPage<Empower>> getGoodsTypeList(@RequestBody String selectInfo) throws Exception {
        SelectPageEntity<Empower> selectPageEntity = new SelectPageEntity<>(selectInfo);
        BlogAction.logger.info("企业" + selectPageEntity.getCompanyId() + "-----获取所有授权信息");
        ReturnItem<IPage<Empower>> returnItem = new ReturnItem<>();
        returnItem.setT(empowerService.getEmpowerList(selectPageEntity));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(SELECT_GOODS_TYPE_INFO_SUCCESS);
        return returnItem;
    }
}

