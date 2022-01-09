package com.bcdm.foodtraceability.entity;

import java.time.LocalDateTime;
import java.io.Serializable;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * <p>
 * 商品类别
 * </p>
 *
 * @author 王
 * @since 2022-01-09
 */
@Data
  @EqualsAndHashCode(callSuper = false)
    public class GoodsType implements Serializable {

    private static final long serialVersionUID=1L;

      private Integer goodsTypeId;

    private String goodsTypeName;

    private LocalDateTime createTime;

    private LocalDateTime updateTime;


}
