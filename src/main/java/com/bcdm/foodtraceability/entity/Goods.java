package com.bcdm.foodtraceability.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;

import java.time.LocalDate;
import java.time.LocalDateTime;
import com.baomidou.mybatisplus.annotation.TableField;
import java.io.Serializable;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

/**
 * <p>
 * 商品信息载体
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@Getter
@Setter
@EqualsAndHashCode(callSuper = false)
public class Goods implements Serializable {

    private static final long serialVersionUID=1L;

    /**
     * 商品ID
     */
    @TableId(value = "goods_id", type = IdType.AUTO)
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
    @TableField("goods_type_id")
    private Integer goodsTypeId;

    /**
     * 企业ID
     */
    private Integer companyId;

    /**
     * 供应商ID
     */
    private Integer supplierId;

    /**
     * 生产厂商ID
     */
    private Integer manufacturerId;

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
