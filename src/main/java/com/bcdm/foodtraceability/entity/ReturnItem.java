package com.bcdm.foodtraceability.entity;

import lombok.Data;

/**
 * <p>
 * 页面返回类
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@Data
public class ReturnItem<T> {

    /**
     * 返回页面数据
     */
    private T t;

    /**
     * 返回页面状态值
     */
    private String httpStatus;

    /**
     * 返回页面信息
     */
    private String httpMessage;

}
