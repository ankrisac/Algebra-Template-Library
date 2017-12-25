#pragma once


#include<functional>
#include<sstream>
#include<vector>


namespace atl{ 
	namespace mtl {
		const std::string excep_generic       = "_ATL_Matrix";
		const std::string excep_out_of_range  = excep_generic + "_out_of_range";
		const std::string excep_conflict_size = excep_generic + "_size_conflict";

		inline size_t encode_row_major(size_t row, size_t col, size_t num_col) {
			return (row * num_col) + col;
		}

		template<typename T> class matrix {
		public:
			matrix(const size_t rows = 0, const size_t cols = 0, const T value = T()) {
				resize(rows, cols, value);
			}
			~matrix() {

			}
		private:
			T& unsafe_get(size_t row, size_t col) {
				return m_elements[encode_row_major(row, col, m_num_cols)];
			}
			const T& unsafe_get(size_t row, size_t col) const {
				return m_elements[encode_row_major(row, col, m_num_cols)];
			}

			matrix<T> unsafe_get_row(size_t row) const {
				matrix<T> out_vec(1, m_num_cols);

				for (size_t i = 0; i < m_num_cols; i++) {
					out_vec.unsafe_get(0, i) = unsafe_get(row, i);
				}
				return out_vec;
			}
			matrix<T> unsafe_get_col(size_t col) const {
				matrix<T> out_vec(m_num_rows, 1);

				for (size_t i = 0; i < m_num_rows; i++) {
					out_vec.unsafe_get(i, 0) = unsafe_get(i, col);
				}
				return out_vec;
			}
			void unsafe_set_row(matrix<T> row_vec, size_t row) {
				for (size_t i = 0; i < m_num_cols; i++) {
					unsafe_get(row, i) = row_vec.unsafe_get(0, i);
				}
			}
			void unsafe_set_col(matrix<T> col_vec, size_t col) {
				for (size_t i = 0; i < m_num_rows; i++) {
					unsafe_get(i, col) = col_vec.unsafe_get(i, 0);
				}
			}

			matrix<T>& unsafe_add(const matrix<T> &mat) {
				for (size_t i = 0; i < m_num_rows * m_num_cols; i++) {
					m_elements[i] += mat.m_elements[i];
				}
				return *this;
			}
			matrix<T>& unsafe_sub(const matrix<T> &mat) {
				for (size_t i = 0; i < m_num_rows * m_num_cols; i++) {
					m_elements[i] -= mat.m_elements[i];
				}
				return *this;
			}
			matrix<T>& unsafe_mul(const matrix<T> &mat) {
				size_t col = mat.m_num_cols;
				std::vector<T> cache(m_num_rows * col);
				T sum;

				for (size_t i = 0; i < m_num_rows; i++) {
					for (size_t j = 0; j < col; j++) {
						for (size_t k = 0; k < m_num_cols; k++) {
							cache[encode_row_major(i, j, col)] += (unsafe_get(i, k) * mat.unsafe_get(k, j));
						}
					}
				}

				m_elements = cache;
				m_num_rows = m_num_rows;
				m_num_cols = col;

				return *this;
			}
		public:
			T& at(size_t row, size_t col) {
				if (row < m_num_rows && col < m_num_cols) {
					return unsafe_get(row, col);
				}
				throw std::logic_error(excep_out_of_range);
			}
			const T& at(size_t row, size_t col) const {
				if (row < m_num_rows && col < m_num_cols) {
					return unsafe_get(row, col);
				}
				throw std::logic_error(excep_out_of_range);
			}
			T& operator()(size_t row, size_t col) {
				return at(row, col);
			}
			const T& operator()(size_t row, size_t col) const {
				return at(row, col);
			}

			size_t get_num_rows() {
				return m_num_rows;
			}
			size_t get_num_cols() {
				return m_num_cols;
			}

			matrix<T> get_row(size_t row) const {
				if (row < m_num_rows) {
					return unsafe_get_row(row);
				}
				else {
					throw std::logic_error(excep_out_of_range);
				}
			}
			matrix<T> get_col(size_t col) const {
				if (col < m_num_cols) {
					return unsafe_get_col(col);
				}
				else {
					throw std::logic_error(excep_out_of_range);
				}
			}
			
			void set_row(matrix<T> row_vec, size_t row) {
				if (row < m_num_rows) {
					if (row_vec.get_num_rows() == 1 && row_vec.get_num_cols() == m_num_cols) {
						unsafe_set_row(row_vec, row);
					}
					else {
						throw std::logic_error(excep_conflict_size);
					}
				}
				else {
					throw std::logic_error(excep_out_of_range);
				}
			}
			void set_col(matrix<T> col_vec, size_t col) {
				if (col < m_num_cols) {
					if (col_vec.get_num_rows() == m_num_rows && col_vec.get_num_cols() == 1) {
						unsafe_set_col(col_vec, col);
					}
					else {
						throw std::logic_error(excep_conflict_size);
					}
				}
				else {
					throw std::logic_error(excep_out_of_range);
				}
			}

			void apply_func(std::function<T(size_t)> func) {
				for (size_t i = 0; i < m_num_rows * m_num_cols; i++) {
					m_elements[i] = func(i);
				}
			}
			void apply_func(std::function<T(size_t, size_t)> func) {
				for (size_t i = 0; i < m_num_rows; i++) {
					for (size_t j = 0; j < m_num_cols; j++) {
						unsafe_get(i, j) = func(i, j);
					}
				}
			}
			void apply_func(std::function<T(size_t, size_t, T)> func) {
				for (size_t i = 0; i < m_num_rows; i++) {
					for (size_t j = 0; j < m_num_cols; j++) {
						unsafe_get(i, j) = func(i, j, unsafe_get(i, j));
					}
				}
			}

			matrix<T>& operator+=(const matrix<T> &mat) {
				if (m_num_rows == mat.m_num_rows && m_num_cols == mat.m_num_cols) {
					unsafe_add(mat);
					return *this;
				}
				else {
					throw std::logic_error(excep_conflict_size);
				}
			}
			matrix<T>& operator-=(const matrix<T> &mat) {
				if (m_num_rows == mat.m_num_rows && m_num_cols == mat.m_num_cols) {
					unsafe_sub(mat);
					return *this;
				}
				else {
					throw std::logic_error(excep_conflict_size);
				}
			}
			matrix<T>& operator*=(const matrix<T> &mat) {
				if (m_num_cols == mat.m_num_rows) {
					unsafe_mul(mat);
					return *this;
				}
				else {
					throw std::logic_error(excep_conflict_size);
				}
			}
			matrix<T>& operator*=(const T &value) {
				for (size_t i = 0; i < m_num_rows * m_num_cols; i++) {
					m_elements[i] *= value;
				}
				return *this;
			}

			matrix<T> transpose() const {
				matrix<T> out_mat(m_num_cols, m_num_rows);
				for (size_t i = 0; i < m_num_rows; i++) {
					for (size_t j = 0; j < m_num_cols; j++) {
						out_mat.unsafe_get(j, i) = unsafe_get(i, j);
					}
				}

				return out_mat;
			}

			std::string str(const std::string sep = ",", const std::string begin = "[", const std::string end = "]\n") const {
				std::stringstream out;
				for (size_t i = 0; i < m_num_rows; i++) {
					out << begin;
					for (size_t j = 0; j < m_num_cols - 1; j++) {
						out << unsafe_get(i, j) << sep;
					}
					out << unsafe_get(i, m_num_cols - 1) << end;
				}

				return out.str();
			}
			void resize(const size_t rows, const size_t cols, const T &value = T()) {
				m_num_rows = rows;
				m_num_cols = cols;
				m_elements.resize(rows * cols, value);
				m_elements.shrink_to_fit();
			}
		private:
			size_t m_num_rows;
			size_t m_num_cols;

			std::vector<T> m_elements;
		};

		template<typename T> 
		std::ostream& operator<<(std::ostream &out, const matrix<T> &mat) {
			return out << mat.str();
		}
		template<typename T> 
		inline matrix<T> operator+(matrix<T> l_mat, const matrix<T> &r_mat) {
			l_mat += r_mat;
			return l_mat;
		}
		template<typename T> 
		inline matrix<T> operator-(matrix<T> l_mat, const matrix<T> &r_mat) {
			l_mat -= r_mat;
			return l_mat;
		}
		template<typename T> 
		inline matrix<T> operator*(matrix<T> l_mat, const matrix<T> &r_mat) {
			l_mat *= r_mat;
			return l_mat;
		}
		template<typename T> 
		inline matrix<T> operator*(matrix<T> l_mat, const T &r_val) {
			l_mat *= r_val;
			return l_mat;
		}
		template<typename T> 
		inline matrix<T> operator*(const T &l_val, matrix<T> r_mat) {
			r_mat *= l_val;
			return r_mat;
		}
		
		
		template<typename T, size_t R, size_t C> class f_matrix {
		public:
			f_matrix(T value = T()) {
				m_matrix.resize(R, C, value);
			}
			~f_matrix() {

			}
		private:
			template<typename _T, size_t _R, size_t _C> friend class f_matrix;
			friend class matrix<T>;
		public:
			T& at(size_t row, size_t col) {
				return m_matrix.at(row, col);
			}
			const T& at(size_t row, size_t col) const {
				return m_matrix.at(row, col);
			}
			T& operator()(size_t row, size_t col) {
				return at(row, col);
			}
			const T& operator()(size_t row, size_t col) const {
				return at(row, col);
			}
			
			explicit operator const matrix<T>() const {
				return m_matrix;
			}
	
			f_matrix<T, 1, C> get_row(size_t row) {
				f_matrix<T, 1, C> out_vec;
				out_vec.m_matrix = m_matrix.get_row(row);

				return out_vec;
			}
			f_matrix<T, R, 1> get_col(size_t col) {
				f_matrix<T, R, 1> out_vec;
				out_vec.m_matrix = m_matrix.get_col(col);

				return out_vec;
			}
			void set_row(const f_matrix<T, 1, C> &row_vec, size_t row) {
				m_matrix.set_row(static_cast<matrix<T>>(row_vec), row);
			}
			void set_col(const f_matrix<T, R, 1> &col_vec, size_t col) {
				m_matrix.set_col(static_cast<matrix<T>>(col_vec), col);
			}

			void apply_func(std::function<T(size_t)> func) {
				m_matrix.apply_func(func);
			}
			void apply_func(std::function<T(size_t, size_t)> func) {
				m_matrix.apply_func(func);
			}
			void apply_func(std::function<T(size_t, size_t, T)> func) {
				m_matrix.apply_func(func);
			}

			f_matrix<T, R, C> operator+=(const f_matrix<T, R, C> &mat) {
				m_matrix += static_cast<matrix<T>>(mat);
				return *this;
			}
			f_matrix<T, R, C> operator-=(const f_matrix<T, R, C> &mat) {
				m_matrix -= static_cast<matrix<T>>(mat);
				return *this;
			}
			f_matrix<T, R, C> operator*=(const f_matrix<T, C, C> &mat) {
				m_matrix *= static_cast<matrix<T>>(mat);
				return *this;
			}
			f_matrix<T, R, C> operator*=(const T &value) {
				m_matrix *= value;
				return *this;
			}
			template<size_t CR> 
			static f_matrix<T, R, C> mult(const f_matrix<T, R, CR> &mat_1, const f_matrix<T, CR, C> &mat_2) {
				f_matrix<T, R, C> out_mat;
				out_mat.m_matrix = static_cast<matrix<T>>(mat_1) * static_cast<matrix<T>>(mat_2);
			
				return out_mat;
			}

			f_matrix<T, C, R> transpose() const {
				f_matrix<T, C, R> out_mat;
				out_mat.m_matrix = m_matrix.transpose();
				return out_mat;
			}

			std::string str(const std::string sep = ",", const std::string begin = "[", const std::string end = "]\n") const {
				return m_matrix.str();
			}
		protected:
		private:
			matrix<T> m_matrix;
		};
		
		template<typename T, size_t R, size_t C>
		std::ostream& operator<<(std::ostream &out, const f_matrix<T, R, C> &mat) {
			return out << mat.str();
		}
		template<typename T, size_t R, size_t C> 
		inline f_matrix<T, R, C> operator+(f_matrix<T, R, C> l_mat, const f_matrix<T, R, C> &r_mat) {
			l_mat += r_mat;
			return l_mat;
		}
		template<typename T, size_t R, size_t C>
		inline f_matrix<T, R, C> operator-(f_matrix<T, R, C> l_mat, const f_matrix<T, R, C> &r_mat) {
			l_mat -= r_mat;
			return l_mat;
		}
		template<typename T, size_t R, size_t CR, size_t C> 
		inline f_matrix<T, R, C> operator*(const f_matrix<T, R, CR> &l_mat, const f_matrix<T, CR, C> &r_mat) {
			return f_matrix<T, R, C>::mult(l_mat, r_mat);
		}
		template<typename T, size_t R, size_t C> 
		inline f_matrix<T, R, C> operator*(f_matrix<T, R, C> l_mat, const T &r_val) {
			l_mat *= r_val;
			return l_mat;
		}
		template<typename T, size_t R, size_t C> 
		inline f_matrix<T, R, C> operator*(const T &l_val, f_matrix<T, R, C> r_mat) {
			r_mat *= l_val;
			return r_mat;
		}
	}
}
