package com.bcdm.foodtraceability.entity;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

import java.time.LocalDate;
import java.time.LocalDateTime;

@Getter
@Setter
@EqualsAndHashCode(callSuper = false)
public class GoodsModel {

    /**
     * 商品ID
     */
    private Integer goodsId;

    /**
     * 商品状态
     */
    private Integer goodsStatus;

    /**
     * 商品级别
     */
    private Integer goodsLevel;

    /**
     * 商品类别ID
     */
    private Integer goodsTypeId;

    /**
     * 商品类别名称
     */
    private String goodsTypeName;

    /**
     * 企业ID
     */
    private Integer companyId;

    /**
     * 供应商ID
     */
    private Integer supplierId;

    /**
     * 供应商名称
     */
    private String supplierName;

    /**
     * 生产厂商ID
     */
    private Integer manufacturerId;

    /**
     * 生产厂商名称
     */
    private String manufacturerName;

    /**
     * 商品名称
     */
    private String goodsName;

    /**
     * 商品说明
     */
    private String goodsExplain;

    /**
     * 原材料
     */
    private String rawMaterial;

    /**
     * 保质期
     */
    private LocalDate qualityGuarantee;

    /**
     * 生产日期
     */
    private LocalDate manufactureDate;

    /**
     * 生产产地编号
     */
    private Integer placeOfProduction;

    /**
     * 生产产地名称
     */
    private String placeOfProductionName;

    /**
     * 条形码编号
     */
    private String barcodeNumber;

    /**
     * 商品图片
     */
    private String productIcon;

    private LocalDateTime updateTime;
}
