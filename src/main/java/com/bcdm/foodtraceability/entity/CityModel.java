package com.bcdm.foodtraceability.entity;


import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

import java.util.List;

/**
 * <p>
 * 城市信息载体Entity
 * </p>
 *
 * @author 王
 * @since 2022-02-12
 */
@Getter
@Setter
@EqualsAndHashCode(callSuper = false)
public class CityModel {

    /**
     * 省编号/城市编号
     */
    private Integer id;

    /**
     * 省份名称/城市名称
     */
    private String name;

    /**
     * 省份内城市list
     */
    List<CityModel> children;
}
