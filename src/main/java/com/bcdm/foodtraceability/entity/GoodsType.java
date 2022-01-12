package com.bcdm.foodtraceability.entity;

import java.time.LocalDateTime;
import java.io.Serializable;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * <p>
 * 
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@Data
@EqualsAndHashCode(callSuper = false)
public class GoodsType implements Serializable {

    private static final long serialVersionUID=1L;

    /**
     * 商品类别ID
     */
    private Integer goodsTypeId;

    /**
     * 商品类别名称
     */
    private String goodsTypeName;

    private LocalDateTime createTime;

    private LocalDateTime updateTime;


}
