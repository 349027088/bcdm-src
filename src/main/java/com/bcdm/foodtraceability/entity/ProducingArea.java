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
 * @since 2022-01-11
 */
@Data
@EqualsAndHashCode(callSuper = false)
public class ProducingArea implements Serializable {

    private static final long serialVersionUID=1L;

    /**
     * 产地ID
     */
    private Integer producingAreaId;

    /**
     * 产地省份
     */
    private String producingProvince;

    /**
     * 产地市
     */
    private String producingCity;

    private LocalDateTime createTime;

    private LocalDateTime updateTime;


}
