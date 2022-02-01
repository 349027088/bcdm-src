package com.bcdm.foodtraceability.common;

/**
 * <p>
 * 系统常量配置类
 * </p>
 *
 * @author 王
 * @since 2022-01-09
 */
public class Constants {


    public static final Integer SELECT_CHECK_PARAM_CREATE = 1;

    public static final Integer SELECT_CHECK_PARAM_MODIFY = 2;

    public static final Integer SELECT_CHECK_PARAM_DELETE = 3;

    public static final Integer SELECT_CHECK_PARAM_GET = 4;

    /** 性别男  */
    public static final Integer USER_SEX_MAN = 1;

    /** 性别女  */
    public static final Integer USER_SEX_WOMEN = 0;

    /** 普通管理员级别  */
    public static final Integer MANAGEMENT_NORMAL_LEVEL = 1;

    /** 管理员全级别  */
    public static final Integer MANAGEMENT_HIGH_LEVEL = 2;

    /** 查询结果为0  */
    public static final Integer SELECT_ZERO = 0;

    /** 插入一条数据  */
    public static final Integer GET_ONE = 1;

    /** 单个用户公司创建上限 */
    public static final Integer COMPANY_MAX = 3;

    /** . */
    public static final String CUT_POINT = ".";

    /** 系统自动创建管理员编号 */
    public static final Integer DEFAULT_MANAGEMENT_ID = 0;

    /** 默认存入金额 */
    public static final Integer DEFAULT_MONEY_AMT = 0;

    /** 默认启动时限 */
    public static final Integer DEFAULT_USAGE_TIME = 31;


    // --------------------------------管理员状态--------------------------------

    /** 解锁状态  */
    public static final Integer MANAGEMENT_STATUS_UNLOCK = 0;
    // --------------------------------用户状态--------------------------------

    /** 解锁状态  */
    public static final Integer USER_STATUS_UNLOCK = 0;

    /** 加锁状态  */
    public static final Integer USER_STATUS_LOCK = 1;

    // --------------------------------企业状态--------------------------------

    /** 企业状态：未提供服务 */
    public static final Integer COMPANY_STATUS_OUT_OF_SERVICE = 0;

    /** 企业状态：正在提供服务 */
    public static final Integer COMPANY_STATUS_ON_SERVICE = 1;

    /** 企业状态：企业正在被锁定 */
    public static final Integer COMPANY_STATUS_IS_LOCK = 2;

    /** 企业状态：企业关闭 */
    public static final Integer COMPANY_STATUS_CLOSE = 3;

    // --------------------------------企业状态--------------------------------

    /** 企业状态：未提供服务 */
    public static final Integer COMPANY_LEVEL_NORMAL = 0;

    /** 企业状态：未提供服务 */
    public static final Integer COMPANY_LEVEL_HIGH_ = 1;

    // --------------------------------关联状态--------------------------------

    /** 企业申请中 */
    public static final Integer COMPANY_USER_WAIT = 99;

    /** 企业老板 */
    public static final Integer COMPANY_USER_0 = 0;

    /** 管理员 */
    public static final Integer COMPANY_USER_1 = 1;

    /** 普通员工 */
    public static final Integer COMPANY_USER_2 = 2;

    /** 待审批员工 */
    public static final Integer COMPANY_USER_3 = 3;

    // -------------------------------供应商锁定确认状态--------------------------------

    /** 供应商正常 */
    public static final Integer SUPPLIER_STATUS_ON_SERVICE = 0;

    /** 供应商异常 */
    public static final Integer SUPPLIER_STATUS_OUT_OF_SERVICE = 1;

    // -------------------------------供应商服务提供状态--------------------------------

    /** 供应商提供服务 */
    public static final Integer SUPPLIER_LEVEL_ON_SERVICE = 0;

    /** 供应商被删除 */
    public static final Integer SUPPLIER_LEVEL_OUT_OF_SERVICE = 1;

    // -------------------------------商品状态--------------------------------

    /** 商品上架 */
    public static final Integer GOODS_STATUS_ON_SERVICE = 0;

    /** 商品下架 */
    public static final Integer GOODS_STATUS_OUT_OF_SERVICE = 1;

    // -------------------------------商品级别--------------------------------

    /** 商品级别0 */
    public static final Integer GOODS_LEVEL_ZERO = 0;

    /** 商品级别1 */
    public static final Integer GOODS_LEVEL_ONE = 1;
}
