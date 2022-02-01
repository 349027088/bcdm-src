package com.bcdm.foodtraceability.common;

/**
 * <p>
 * 页面返回信息常量配置类
 * </p>
 *
 * @author 王
 * @since 2022-01-16
 */
public class MessageConstants {

    /** 服务器异常 */
    public static final String SEVER_ERROR = "服务器异常";

    /** 空指针异常 */
    public static final String NOT_POINT_ERROR = "空指针异常";

    /** 运行时异常 */
    public static final String RUNTIME_ERROR = "运行时异常";

    /** 登录成功 */
    public static final String LOGIN_SUCCESS = "登录成功";

    /** 账号密码错误 */
    public static final String LOGIN_FAIL = "当前账号不存在或者输入的密码有误";

    /** 注册成功 */
    public static final String REGISTER_SUCCESS = "注册成功";

    /** 当前账号已被使用 */
    public static final String REGISTER_FAIL = "当前账号已被使用";

    /** 密码修改成功 */
    public static final String MODIFY_PASSWORD_SUCCESS = "密码修改成功";

    /** 密码修改失败 */
    public static final String MODIFY_PASSWORD_FAIL = "密码修改失败";

    /** 修改用户信息失败 */
    public static final String MODIFY_USERINFO_FAIL = "修改用户信息失败";

    /** 用户锁定失败 */
    public static final String LOCK_USER_FAIL = "用户锁定失败";

    /** 用户解锁失败 */
    public static final String UNLOCK_USER_FAIL = "用户解锁失败";

    /** 没有获得用户绑定的公司信息 */
    public static final String USER_GET_COMPANY_INFO_FAIL = "没有获得用户绑定的公司信息";

    /** 员工信息获取失败 */
    public static final String COMPANY_GET_USER_INFO_FAIL = "员工信息获取失败";

    /** 企业登录成功 */
    public static final String CREATE_COMPANY_SUCCESS = "企业登录成功";

    /** 企业信息修改成功 */
    public static final String MODIFY_COMPANY_INFO_SUCCESS = "企业信息修改成功";

    /** 员工信息获取成功 */
    public static final String COMPANY_USER_GET_SUCCESS = "员工信息获取成功";

    /** 企业图片上传成功 */
    public static final String ICON_UPLOAD_SUCCESS = "企业图片上传成功";

    /** 修改员工职位成功 */
    public static final String MODIFY_USER_TO_COMPANY_SUCCESS = "修改员工职位成功";

    /** 登录到企业成功 */
    public static final String CREATE_USER_TO_COMPANY_SUCCESS = "登录到企业成功";

    /** 添加企业信息失败 */
    public static final String CREATE_COMPANY_FAIL = "添加企业信息失败";

    /** 企业信息获取成功 */
    public static final String GET_COMPANY_INFO_SUCCESS = "企业信息获取成功";

    /** 修改企业信息失败 */
    public static final String MODIFY_COMPANY_FAIL = "修改企业信息失败";

    /** 图片类型不符合规范 */
    public static final String ICON_TYPE_FORMAT_FAIL = "图片类型不符合规范";

    /** 图片上传失败 */
    public static final String ICON_UPLOAD_FAIL = "图片上传失败";

    /** 图片超过大小限制 */
    public static final String ICON_SIZE_FAIL = "图片超过大小限制";

    /** 修改企业授权信息失败*/
    public static final String MODIFY_EMPOWER_FAIL = "修改企业授权信息失败";

    /**企业授权失败*/
    public static final String ENTERPRISE_AUTHORIZATION_FAILED = "企业授权失败";

    /** 创建授权信息失败 */
    public static final String CREATE_EMPOWER_FAIL = "创建授权信息失败";

    /** 创建关联信息失败 */
    public static final String CREATE_JURISDICTION_FAIL = "创建关联信息失败";

    /** 修改权限信息失败 */
    public static final String MODIFY_JURISDICTION_FAIL = "修改权限信息失败";

    /** 修改权限级别不足 */
    public static final String MODIFY_JURISDICTION_LEVEL_FAIL = "修改权限级别不足";

    /** 创建供应商失败 */
    public static final String CREATE_SUPPLIER_FAILED = "创建供应商失败";

    /** 修改供应商信息失败 */
    public static final String MODIFY_SUPPLIER_FAILED = "修改供应商信息失败";

    /** 查询授权信息失败 */
    public static final String SELECT_EMPOWER_FAIL = "查询授权信息失败";

    /** 查询供应商信息失败 */
    public static final String SELECT_SUPPLIER_INFO_FAIL = "查询供应商信息失败";

    /** 删除供应商信息失败 */
    public static final String DELETE_SUPPLIER_INFO_FAIL = "删除供应商信息失败";

    /** 正在等待企业承认您的申请 */
    public static final String USER_ADMIT_FAIL = "正在等待企业承认您的申请";

    /** 获取供应商信息成功 */
    public static final String SELECT_SUPPLIER_INFO_SUCCESS = "获取供应商信息成功";

    /** 添加供应商信息成功 */
    public static final String ADD_SUPPLIER_INFO_SUCCESS = "查询授权信息失败";

    /** 修改供应商信息成功 */
    public static final String MODIFY_SUPPLIER_INFO_SUCCESS = "修改供应商信息成功";

    /** 删除供应商信息成功 */
    public static final String DELETE_SUPPLIER_INFO_SUCCESS = "删除供应商信息成功";

    /** 添加商品种类信息失败 */
    public static final String ADD_GOODS_TYPE_FAIL = "添加商品种类信息失败";

    /** 删除商品种类信息失败 */
    public static final String DELETE_GOODS_TYPE_FAIL = "删除商品种类信息失败";

    /** 获取商品种类信息成功 */
    public static final String SELECT_GOODS_TYPE_INFO_SUCCESS = "获取商品种类信息成功";

    /** 获取商品种类信息失败或者查出0条 */
    public static final String SELECT_GOODS_TYPE_INFO_FAIL = "获取商品种类信息失败或当前没有设置商品种类信息";

    /** 删除商品种类信息成功 */
    public static final String DELETE_GOODS_TYPE_SUCCESS = "删除商品种类信息成功";

    /** 增加商品种类信息成功 */
    public static final String ADD_GOODS_TYPE_SUCCESS = "增加商品种类信息成功";

    /** 修改商品种类信息成功 */
    public static final String MODIFY_GOODS_TYPE_SUCCESS = "修改商品种类信息成功";

    /** 当前种类名称不存在 */
    public static final String FIND_GOODS_TYPE_NAME_BY_COMPANY_FAIL1 = "当前种类名称不存在";

    /** 当前种类名称已经存在 */
    public static final String FIND_GOODS_TYPE_NAME_BY_COMPANY_FAIL2 = "当前种类名称已经存在";

    /** 修改种类名称失败 */
    public static final String MODIFY_GOODS_TYPE_FAIL = "修改种类名称失败";
}
