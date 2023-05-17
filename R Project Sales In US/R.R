#Chuyển đổi Working Direction và Read data
setwd("C:/Users/Admin/OneDrive/ĐỒ ÁN NHÓM 3.zip")
sales_data <- read.csv("sales3.csv") 
#Khai báo các thư viện
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyverse)
# Xem kích thước của dữ liệu
(sales_data)
names(sales_data)
# Xem 5 dòng đầu tiên của dữ liệu
head(sales_data, 5)

# Xem 2 dòng cuối cùng của dữ liệu
tail(sales_data, 2)

#
##kiếm định biến total và biến ordered
# Tạo biến nhóm dựa trên dưới 100
sales_data$qty_group <- ifelse(sales_data$qty_ordered < 100, "Group 1", "Group 2")

# Kiểm định t giữa hai nhóm
t.test(total ~ qty_group, data = sales_data)
ggplot(sales_data, aes(x = qty_ordered, y = total)) +
  geom_point() +
  scale_x_log10()

ggplot(sales_data, aes(x = qty_ordered, y = total)) +
  geom_point()
#Kiểm định 2 biến giới tính nam và nữ phụ thuộc vào số lượng đặt hàng hay không
class(sales_data$Gender)
sales_data$Gender_numeric <- ifelse(sales_data$Gender == "M", 1, 2)
male_sales <- sales_data %>% filter(Gender == "M") %>% select(qty_ordered)
female_sales <- sales_data %>% filter(Gender == "F") %>% select(qty_ordered)
t.test(male_sales$qty_ordered, female_sales$qty_ordered)
# tính 
sales_summary <- sales_data %>% group_by(Gender) %>% summarize(total_qty = sum(qty_ordered))

# Vẽ biểu đồ cột thể hiện tổng số lượng đặt hàng của 2 giới tính
ggplot(sales_summary, aes(x = Gender, fill = Gender)) +
  geom_bar()+
  labs(title = "Tổng số lượng đặt hàng của nam và nữ",
       x = "Giới tính",
       y = "Tổng số lượng đặt hàng") +
  theme_minimal()

#Xem thông tin các cột
names(sales_data)

# Vẽ biểu đồ scatterplot thể hiện mối quan hệ giữa discount_amount và total_price
ggplot(sales_data, aes(x=discount_amount, y=total)) +
  geom_point(alpha=0.5) +
  labs(title="Relationship between Discount Amount and Total Price", x="Discount Amount", y="Total Price")
# Tạo bảng tần số cho hai biến: category và region
table <- table(sales_data$category, sales_data$Region)

#kiểm định Chi-square
chisq.test(table)
#Kiểm định Anova
anova <- aov(qty_ordered ~ category, data = sales_data)
summary(anova)
# Vẽ boxplot cho từng nhóm
ggplot(sales_data, aes(x = category, y = qty_ordered)) +
  geom_boxplot() +
  labs(title = "Phân phối số lượng đặt hàng theo category",
       x = "Category",
       y = "Số lượng đặt hàng") +
  theme_minimal()

# Vẽ biểu đồ cột thể hiện trung bình số lượng đặt hàng của từng nhóm
ggplot(sales_data, aes(x = category, y = qty_ordered)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Trung bình số lượng đặt hàng theo category",
       x = "Category",
       y = "Trung bình số lượng đặt hàng") +
  theme_minimal()
#Vẽ biểu đồ thể hiện số lượng đặt hàng theo loại sản phẩm

selected_category <- c("Appliances","Computing","Mobiles & Tablets","Others","Superstore","Entertainment")
category_data_selected <- subset(sales_data, category %in% selected_category)
# Vẽ biểu đồ cột thể hiện trung bình số lượng đặt hàng của từng nhóm
ggplot(category_data_selected, aes(x = category, y = qty_ordered,fill = category)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Trung bình số lượng đặt hàng theo category",
       x = "Category",
       y = "Trung bình số lượng đặt hàng") +
  theme_minimal()

#Kiểm định tương quang
cor.test(sales_data$unit_price, sales_data$qty_ordered)
####Phân tích xu hướng bán hàng theo thời gian
sales_data$month <- month(sales_data$order_date)
# Phân nhóm theo tháng và tính tổng doanh số
monthly_sales <- aggregate(total ~ month, data = sales_data, FUN = sum)
names(monthly_sales) <- c("month", "total_sales")
# Vẽ biểu đồ đường
ggplot(monthly_sales, aes(x = month, y = total_sales, group = 1)) +
  geom_line(color = "#3b7dd8", size = 1) +  # thay đổi màu sắc và độ dày cho đường
  labs(title = "Tổng doanh số bán hàng theo tháng",
       x = "Tháng",
       y = "Tổng doanh số (USD)") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  
        axis.title = element_text(size = 16, face = "bold"),  
        axis.text = element_text(size = 14)) +  
  scale_x_discrete(breaks = 1:12,labels = month.name[1:12])  # đổi tên các tháng từ số sang chữ


####Biểu đồ tròn thể hiện theo biến category
category <- sales_data %>% 
  group_by(category) %>%
  summarize(Total = sum(qty_ordered)) %>%
  arrange(desc(Total))
# Tính tỷ lệ phần trăm của từng danh mục và chỉ giữ lại danh mục có tổng số cao nhất
category_pct <- category %>%
mutate(Percent = round(Total/sum(Total)*100, 2)) %>%
filter(row_number() == 1)

#Tính top category bán nhiều nhất 
top_category <- category[1,]$category
top_sales <- category[1,]$Total
total_sales <- sum(category$Total)

ggplot(category, aes(x="", y= Total, fill=category)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme_void() +
  labs(fill="Category") +
  ggtitle("Sales by Category") +
  annotate("text", x=0, y=0, label=paste0(top_category, ": ", round(top_sales/total_sales*100,1), "%"))

##Biểu đồ tròn thể hiện theo biến khu vực
Region <- sales_data %>% 
  group_by(Region) %>%
  summarize(Total = sum(qty_ordered)) %>%
  arrange(desc(Total))

ggplot(Region, aes(x="", y=Total, fill=Region)) +
    geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme_void() +
  labs(fill="Region") +
  geom_text(aes(label = paste0(round(Total/sum(Total)*100), "%")), position = position_stack(vjust = 0.5)) +
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        plot.title=element_text(size=14, face="bold", margin=margin(b=10)),
        plot.subtitle=element_text(size=12, margin=margin(b=10)))
# Doanh Thu năm
yearly_sales <- aggregate(sales_data$total, by=list(Year=sales_data$year), sum)
ggplot(yearly_sales, aes(x=Year, y=x)) + 
  geom_line(color="green") +
  labs(title = "Yearly Sales", x = "Year", y = "Sales")
##
# Tính tổng số lượng đặt hàng cho mỗi sản phẩm
product_sales <- aggregate(qty_ordered ~ category, data=sales_data, sum)

# Sắp xếp theo thứ tự giảm dần
product_sales <- product_sales[order(product_sales$qty_ordered, decreasing=TRUE),]

# Chọn 20 sản phẩm có số lượng đặt hàng lớn nhất
top_products <- head(product_sales, n=20)

# Vẽ biểu đồ cột
ggplot(top_products, aes(x=category, y=qty_ordered)) +
  geom_col(fill="#4CBB17", alpha=0.8, width=0.7) +
  coord_flip() +
  labs(title="Top 20 Products Ordered", x="Product", y="Total Quantity Ordered") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 20),
        axis.title = element_text(face = "bold", size = 16),
        axis.text = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
##
selected_states <- c("OK", "FL", "ND","WY")
sales_data_selected <- subset(sales_data, State %in% selected_states)

# Vẽ biểu đồ boxplot
ggplot(sales_data_selected, aes(x = State, y = total)) + 
  geom_boxplot() + 
  labs(title = "Sales Data by Selected States", x = "State", y = "Total Sales")
# biểu đồ thể hiện phương pháp thanh toán của các tiểu bang
ggplot(sales_data, aes(x = State, fill = payment_method)) + geom_bar()
#Vì nó nhiều quá nên chúng em đã rút ngọn bằng cách lựa ra các tiểu bang
selected_states <- c("OK", "FL", "ND","WY","MD","IL","IA","ID","MA","MD","ND","GA","AZ","MS","NE")
sales_data_selected <- subset(sales_data, State %in% selected_states)
ggplot(sales_data_selected, aes(x = State, fill = payment_method)) + geom_bar()
labs(title = "payment method by Selected States", x = "State", y = "Payment Method")
#
ggplot(sales_data, aes(x = "", fill = payment_method)) + geom_bar() + coord_polar("y", start=0) + theme_void()
##
ggplot(sales_data, aes(x = Gender, y = age)) +
  geom_boxplot() +
  labs(title = "Box Plot of Age")

##
category <- sales_data %>% 
  group_by(category) %>%
  summarize(Total = sum(qty_ordered)) %>%
  arrange(desc(Total))

top_category <- category[1,]$category
top_sales <- category[1,]$Total
total_sales <- sum(category$Total)

ggplot(category, aes(x="", y= Total, fill=category)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme_void() +
  labs(fill="Category") +
  ggtitle("Sales by Category") +
  annotate("text", x=0, y=0, label=paste0(top_category, ": ", round(top_sales/total_sales*100,1), "%"))
#Dự đoán

sales_by_weekday <- sales_data %>%
  mutate(Date = as.Date(order_date)) %>% # Chuyển đổi cột ngày sang định dạng date
  mutate(Weekday = weekdays(Date)) %>% # Thêm cột ngày của tuần
  group_by(Weekday) %>%
  summarize(TotalSales = sum(total))

# Vẽ biểu đồ dòng cho tổng giá trị bán được theo ngày trong tuần
ggplot(sales_by_weekday, aes(x = Weekday, y = TotalSales, group = 1)) +
  geom_line() +
  ggtitle("Tổng giá trị bán được theo từng ngày trong tuần") +
  xlab("Ngày trong tuần") +
  ylab("Tổng giá trị bán được") +
  theme_minimal()
##
ggplot(sales_data, aes(x = Region,fill = Region)) +
  geom_bar() +
  ggtitle("Số lượng đơn hàng cho từng quốc gia") +
  xlab("Vùng") +
  ylab("Số lượng đơn hàng")
##
ggplot(sales_data, aes(x= "", fill=status)) +
  geom_bar(width = 1) +
  coord_polar("y", start=0) +
  ggtitle("Phân bổ số lượng đơn hàng cho các trạng thái thanh toán") +
  scale_fill_brewer(palette="Set3") +
  theme_void()
##
ggplot(sales_data, aes(x = Region, y = total, fill = Region)) +
  geom_boxplot() +
  ggtitle("Phân tích phân phối chi phí bán hàng theo từng quốc gia") +
  xlab("Quốc gia") +
  ylab("Chi phí bán hàng")
##

bi_st_data <- sales_data %>% 
  filter(!is.na(bi_st)) %>% # Loại bỏ các giá trị thiếu trong bi_st
  group_by(bi_st) %>% 
  summarise(count = n())
#
ggplot(bi_st_data, aes(x = bi_st, y = count)) +
  geom_col(fill = "skyblue") +
  labs(x = "Trạng thái đơn hàng", y = "Số lượng đơn hàng", 
       title = "Phân tích biến bi_st trong Online Sales in USA") +
  theme_minimal()
#
train_data <- sales_data[1:48,]
test_data <- sales_data[49:60,]
# Tiếp theo, chuẩn bị dữ liệu cho ARIMA
train_ts <- ts(train_data$total, frequency = 12)
test_ts <- ts(test_data$total, frequency = 12)

# Sử dụng hàm auto.arima () để xây dựng mô hình ARIMA trên tập train
arima_model <- forecast::auto.arima(train_ts)

# Dự đoán trên tập test sử dụng mô hình ARIMA đã học được
arima_predictions <- forecast::forecast(arima_model, h = 12)

# So sánh dự đoán của mô hình với tập test của dữ liệu thực tế
plot(arima_predictions$mean, type = "l", col = "blue", ylim = range(sales_data$total))+
lines(test_ts, col = "red")+
legend("topright", legend = c("ARIMA predictions", "Test data"), col = c("blue", "red"), lty = 1)
#### Dự đoán
sales_by_weekday <- sales_data %>%
  mutate(Date = as.Date(order_date)) %>% # Chuyển đổi cột ngày sang định dạng date
  mutate(Weekday = weekdays(Date)) %>% # Thêm cột ngày của tuần
  group_by(Weekday) %>%
  summarize(TotalSales = sum(total))

# Vẽ biểu đồ dòng cho tổng giá trị bán được theo ngày trong tuần
ggplot(sales_by_weekday, aes(x = Weekday, y = TotalSales, group = 1)) +
  geom_line() +
  ggtitle("Tổng giá trị bán được theo từng ngày trong tuần") +
  xlab("Ngày trong tuần") +
  ylab("Tổng giá trị bán được") +
  theme_minimal()
