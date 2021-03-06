package com.bcdm.foodtraceability.entity;

import com.bcdm.foodtraceability.validatedgroup.GetInfoGroup;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

import javax.validation.constraints.NotNull;
import java.time.LocalDate;
import java.time.LocalDateTime;

@Getter
@Setter
@EqualsAndHashCode(callSuper = false)
public class GoodsModel {

    public void setGoodsModel() {
        String bufferString = this.companyId + "";
        setGoodsTableName("goods_" + bufferString.substring(bufferString.length() - 1));
        setBarcodeTableName("barcode_" + bufferString.substring(bufferString.length() - 1));
    }

    /**
     * 商品ID
     */
    @NotNull(message = "没有需要查询的商品信息", groups = {GetInfoGroup.class})
    private Integer goodsId;

    /**
     * 商品状态
     */
    private Integer goodsStatus;

    /**
     * goods表名
     */
    private String goodsTableName;

    /**
     * barcode表名
     */
    private String barcodeTableName;

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


    private LocalDateTime createTime;

    private LocalDateTime updateTime;
}
