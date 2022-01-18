package com.bcdm.foodtraceability.controller;

import com.bcdm.foodtraceability.entity.Company;
import com.bcdm.foodtraceability.entity.ReturnItem;
import com.bcdm.foodtraceability.entity.User;
import com.bcdm.foodtraceability.service.CompanyService;
import com.bcdm.foodtraceability.service.IconService;
import com.bcdm.foodtraceability.service.UserService;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_SUCCESS;
import static com.bcdm.foodtraceability.common.MessageConstants.*;

/**
 * <p>
 * 前端控制器
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@RestController
@RequestMapping("/company")
public class CompanyController {

    private final UserService userService;

    private final CompanyService companyService;

    public CompanyController(UserService userService, CompanyService companyService) {
        this.userService = userService;
        this.companyService = companyService;
    }

    /**
     * 企业登录Controller
     *
     * @param user    登录企业的用户
     * @param company 新加入的企业信息
     * @return 创建成功的企业信息
     * @throws Exception 创建失败
     */
    @PostMapping("/register")
    @CrossOrigin
    public ReturnItem<Company> register(@RequestBody User user, Company company) throws Exception {
        ReturnItem<Company> returnItem = new ReturnItem<>();
        returnItem.setT(companyService.register(user, company));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(CREATE_COMPANY_SUCCESS);
        return returnItem;
    }

    /**
     * 企业信息修改Controller
     *
     * @param user    修改信息的用户
     * @param company 需要需改的公司信息
     * @return 修改成功的公司信息
     * @throws Exception 修改信息失败
     */
    @PostMapping("/modify")
    @CrossOrigin
    public ReturnItem<Company> modify(@RequestBody User user, Company company) throws Exception {
        ReturnItem<Company> returnItem = new ReturnItem<>();
        returnItem.setT(companyService.modify(user, company));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(MODIFY_COMPANY_INFO_SUCCESS);
        return returnItem;
    }

    /**
     * 获得公司的用户所有信息
     *
     * @param company 获取用户信息的公司
     * @return 该公司的用户信息
     * @throws Exception 查询用户信息失败
     */
    @GetMapping("/getUserByCompany")
    @CrossOrigin
    public ReturnItem<List<User>> getUserByCompany(@RequestBody Company company) throws Exception {
        ReturnItem<List<User>> returnItem = new ReturnItem<>();
        returnItem.setT(userService.getUserByCompany(company));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(COMPANY_USER_GET_SUCCESS);
        return returnItem;
    }

    /**
     * 创建企业头像图片
     *
     * @param file 需要被创建的企业头像图片
     * @return 创建成功的企业头像地址
     * @throws Exception 企业头像创建失败
     */
    @PostMapping("createCompanyIcon")
    @CrossOrigin
    public ReturnItem<String> createCompanyIcon(@RequestPart MultipartFile file, @RequestPart Company company) throws Exception {
        ReturnItem<String> returnItem = new ReturnItem<>();
        returnItem.setT(companyService.createCompanyIcon(file, company));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(COMPANY_ICON_UPLOAD_SUCCESS);
        return returnItem;
    }

    /**
     * 企业修改企业头像
     *
     * @param file 需要修改成的企业头像
     * @return 返回修改图片的地址
     * @throws Exception 图片修改失败
     */
    @PostMapping("modifyCompanyIcon")
    @CrossOrigin
    public ReturnItem<String> modifyCompanyIcon(@RequestPart MultipartFile file, @RequestPart Company company) throws Exception {
        ReturnItem<String> returnItem = new ReturnItem<>();
        returnItem.setT(companyService.modifyCompanyIcon(file, company));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(COMPANY_ICON_MODIFY_SUCCESS);
        return returnItem;
    }

}

