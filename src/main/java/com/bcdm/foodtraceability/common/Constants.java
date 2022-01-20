package com.bcdm.foodtraceability.common;

/**
 * <p>
 * 常量配置类
 * </p>
 *
 * @author 王
 * @since 2022-01-09
 */
public class Constants {

    /** 性别男  */
    public static final int USER_SEX_MAN = 1;

    /** 性别女  */
    public static final int USER_SEX_WOMEN = 0;

    /** 普通管理员级别  */
    public static final int MANAGEMENT_NORMAL_LEVEL = 1;

    /** 管理员全级别  */
    public static final int MANAGEMENT_HIGH_LEVEL = 2;

    /** 查询结果为0  */
    public static final int SELECT_ZERO = 0;

    /** 单个用户公司创建上限 */
    public static final int COMPANY_MAX = 3;

    /** . */
    public static final String CUT_POINT = ".";

    // --------------------------------管理员状态--------------------------------

    /** 解锁状态  */
    public static final int MANAGEMENT_STATUS_UNLOCK = 0;
    // --------------------------------用户状态--------------------------------

    /** 解锁状态  */
    public static final int USER_STATUS_UNLOCK = 0;

    /** 加锁状态  */
    public static final int USER_STATUS_LOCK = 1;

    // --------------------------------企业状态--------------------------------

    /** 企业状态：未提供服务 */
    public static final int COMPANY_STATUS_OUT_OF_SERVICE = 0;

    /** 企业状态：正在提供服务 */
    public static final int COMPANY_STATUS_ON_SERVICE = 1;

    /** 企业状态：企业正在被锁定 */
    public static final int COMPANY_STATUS_IS_LOCK = 2;

    /** 企业状态：企业关闭 */
    public static final int COMPANY_STATUS_CLOSE = 3;




}
