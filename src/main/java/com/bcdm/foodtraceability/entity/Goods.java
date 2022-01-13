package com.bcdm.foodtraceability.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import java.time.LocalDateTime;
import com.baomidou.mybatisplus.annotation.TableField;
import java.io.Serializable;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * <p>
 * 商品信息载体
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@Data
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
    private Integer goodsTypeId;

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
    private String explain;

    /**
     * 原材料
     */
    private String rawMaterial;

    /**
     * 保质期
     */
    private String qualityGuarantee;

    /**
     * 生产日期
     */
    private LocalDateTime manufactureDate;

    /**
     * 条形码编号
     */
    private Integer barcodeNumber;

    /**
     * 商品图片
     */
    @TableField("Product_icon")
    private String productIcon;

    private LocalDateTime createTime;

    private LocalDateTime updateTime;


}
